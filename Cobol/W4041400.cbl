010200 ID DIVISION.                                                             
010300 PROGRAM-ID.     W4041400.                                                
010400 AUTHOR.         GÖRAN KJELLSON  GUIDE                                    
010500 DATE-WRITTEN.   2004  NOVEMBER                                           
010600 DATE-COMPILED.                                                           
010700                                                                          
010800*    FUNKTION:                                                            
010900*        KUNDREGISTER.GODSMOTTAGARE LDC INFORMATION                       
011000*                                                                         
011100*        PROGRAMMET UPPDATERAR WDB2                                       
011200*                                                                         
011300*    INDATA.                                                              
011400*        TRANSAKTION: W4T414                                              
011500*        MID:         W4I41401                                            
011600*                                                                         
011700*    UTDATA.                                                              
011800*        MOD:         W4O41401                                            
011900*                                                                         
012000*    CHANGE LOG:                                                          
012100*                                                                         
012200*      YY/MM/DD - NAME            - CHANGE DESCRIPTION                    
012300*      ----------------------------------------------------------         
012400*      16/02/12 - REDDY RAHUL     - ADD RFS DATE CALC PARAMETER           
012500*                                   FOR MULTIPLE DC'S.                    
012600*                                   E'TRACKER 10257039                    
012700*                                                                         
012800                                                                          
012900                                                                          
013000 ENVIRONMENT DIVISION.                                                    
013100 DATA DIVISION.                                                           
013200 WORKING-STORAGE SECTION.                                                 
013300                                                                          
013400 77  IDPGM                       PIC X(08)   VALUE 'W4041400'.            
013500                                                                          
013600*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
013700 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
013800                                                                          
013900 77  YES                         PIC X       VALUE 'Y'.                   
014000 77  JA                          PIC X       VALUE 'J'.                   
014100 77  NEJ                         PIC X       VALUE 'N'.                   
014200 77  CURR-SECTION                PIC X(16)   VALUE 'MAIN'.                
014300 77  CURR-IMS-SECTION            PIC X(16).                               
014400                                                                          
014500 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
014600                                                                          
014700 77  INDX                        PIC S9(4)   VALUE ZERO COMP SYNC.        
014801 77  INDX1                       PIC S9(4)   VALUE +0  COMP SYNC.         
014902 77  INDX2                       PIC S9(4)   VALUE +0  COMP SYNC.         
015004 77  INDX-DC                     PIC S9(4)   VALUE ZERO COMP SYNC.        
015104 77  INDX-BULK                   PIC S9(4)   VALUE +0  COMP SYNC.         
015204 77  INDX-DAY                    PIC S9(4)   VALUE +0  COMP SYNC.         
015304 77  INDX-VOR                    PIC S9(4)   VALUE +0  COMP SYNC.         
015408 77  MAX-INDX                    PIC S9(4)   VALUE +8  COMP SYNC.         
015704 77  IX                          PIC S9(9)  VALUE ZERO  COMP SYNC.        
015909 77  SISTA-INDX                  PIC S9(4)   VALUE +0  COMP SYNC.         
016004 77  WS-IDDC-WORK                PIC X(2)    VALUE SPACE.                 
016104 77  WS-DEL-DC                   PIC X(1)    VALUE SPACE.                 
016204 77  WS-BULK-DC-NEW              PIC X(2)    VALUE SPACE.                 
016304 77  WS-DAY-DC-NEW               PIC X(2)    VALUE SPACE.                 
016404 77  WS-VOR-DC-NEW               PIC X(2)    VALUE SPACE.                 
016504 77  WS-TISTADAT                 PIC 9(6)    VALUE ZERO.                  
016604 77  WS-TISTODAT                 PIC 9(6)    VALUE ZERO.                  
016704 77  W-KDORDKL-MIN               PIC S9      COMP-3.                      
016804 77  W-KDORDKL-MAX               PIC S9      COMP-3.                      
016904                                                                          
017004 77  ANTAL-CDC                   PIC 9       VALUE ZERO.                  
017104 77  ANTAL-LDC                   PIC 9       VALUE ZERO.                  
017204 77  ANTAL-SDC                   PIC 9       VALUE ZERO.                  
017304 77  ANTAL-NDC                   PIC 9       VALUE ZERO.                  
017404 77  ANTAL-NDC-SH                PIC 9       VALUE ZERO.                  
017504                                                                          
017604 77  W-WDA5-IDDC                 PIC X(2)    VALUE SPACE.                 
017704                                                                          
017804 01  KINA-SW                     PIC X(1)    VALUE SPACE.                 
017904     88 KINA-DC                              VALUE 'J'.                   
018004                                                                          
018104 77  BETALARE-SW                 PIC X       VALUE 'J'.                   
018204     88  BETALARE-OK                         VALUE 'J'.                   
018304     88  BETALARE-FEL                        VALUE 'N'.                   
018404                                                                          
018504 77  SPEC-SW                     PIC X       VALUE 'J'.                   
018604     88  SPEC-JA                             VALUE 'J'.                   
018704     88  SPEC-NEJ                            VALUE 'N'.                   
018804                                                                          
018904 77  ALLT-SW                     PIC X       VALUE 'J'.                   
019004     88  ALLT-OK                             VALUE 'J'.                   
019104                                                                          
019204 77  UPD-SW                      PIC X       VALUE 'N'.                   
019304     88  UPPDATERING-GJORD                   VALUE 'J'.                   
019404     88  INGEN-UPPDATERING-GJORD             VALUE 'N'.                   
019504                                                                          
019604 01  ORDERKLASS                  PIC X(4)    VALUE SPACE.                 
019704     88 BULK-KL                              VALUE 'BULK'.                
019804     88 DAY-KL                               VALUE 'DAY '.                
019904     88 VOR-KL                               VALUE 'VOR '.                
020004                                                                          
020104 01  W-IDDC-HELP-TABELLER.                                                
020209     03 W-IDDC-BULK OCCURS 8     PIC X(2).                                
020309     03 W-IDDC-DAY  OCCURS 8     PIC X(2).                                
020409     03 W-IDDC-VOR  OCCURS 8     PIC X(2).                                
020504                                                                          
020604 01  W-IDDC-HELP-ORDN.                                                    
020709     03 W-IDDC-ORDN OCCURS 8     PIC X(2).                                
020804                                                                          
020904 01  W-SISTA-DC                  PIC X(2).                                
021004 01  W-NAST-SISTA-DC             PIC X(2).                                
021104 01  TEST-IDDISTR                PIC S9(5)   COMP-3.                      
021204                                                                          
021304*    *** USA/CANADA DISTRIKT ******                                       
021404 01  FILLER REDEFINES TEST-IDDISTR.                                       
021504*    03     -COPY WWDIST07.                                               
021600                                                                          
021700*    *** RETUR- REFILLDISTRIKT ****                                       
021804 01  FILLER REDEFINES TEST-IDDISTR.                                       
021904*    03     -COPY WWDIST35.                                               
022004                                                                          
022104*    *** EJ KONTROLL DISTRIKT *****                                       
022204 01  FILLER REDEFINES TEST-IDDISTR.                                       
022304*    03     -COPY WWDIS108.                                               
022404                                                                          
022500                                                                          
022600*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
022700                                                                          
022800 77  INDATA-SW                   PIC X       VALUE 'J'.                   
022900     88  INDATA-OK                           VALUE 'J'.                   
023000     88  INDATA-FEL                          VALUE 'N'.                   
023100                                                                          
023200 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
023300     88  NYCKLAR-OK                          VALUE 'J'.                   
023400     88  NYCKLAR-FEL                         VALUE 'N'.                   
023504                                                                          
023604 77  DC-FOUND-SW                 PIC X       VALUE 'N'.                   
023704     88  DC-FOUND                            VALUE 'J'.                   
023800                                                                          
023900 77  IDDC-SW                     PIC X       VALUE 'J'.                   
024000     88  IDDC-OK                             VALUE 'J'.                   
024100     88  IDDC-FEL                            VALUE 'N'.                   
024200                                                                          
024300 01  DIVERSE.                                                             
024400     03  WS-IDDC                 PIC X(2).                                
024500                                                                          
024600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
024700     88  EGEN-MID                            VALUE '4414'.                
024800     88  GODK-MID                            VALUE '4411' '4412'          
024900                                                   '4413' '4414'          
025000                                                   '4415' '4416'          
025100                                                   '4417' '4418'          
025200                                                   '4419'.                
025300     88  HELP-MID                            VALUE '0551'.                
025400                                                                          
025504*      --- VALID IDDC CODES                                               
025604*                                                                         
025704*01    -COPY WWDCKONS                                                     
025804*                                                                         
025904*01    -COPY WWDC99                                                       
026004                                                                          
026104                                                                          
026200*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
026300 01  GENERELLA-SUBPROGRAM.                                                
026400     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
026500     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
026604     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
026700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
026800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
026900                                                                          
027000*                                                                         
027100*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
027200*01 -COPY WMEDAREA                                                        
027300                                                                          
027400 01  MESSAGE-CODES.                                                       
027500     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
027600     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
027700     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
027800     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
027904     03  ERR-BO-EXISTS           PIC X(3)    VALUE '361'.                 
028004     03  ERR-ORDER-EXISTS        PIC X(3)    VALUE '366'.                 
028100     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
028200                                                                          
028300                                                                          
028400*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
028500*                                                                         
028600 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
028700*01 -COPY WMSGINIT                                                        
028800                                                                          
028904*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
029004*                                                                         
029104 01  FILLER                      PIC X(16)   VALUE 'WDATKONV'.            
029204                                                                          
029304*01 -COPY WDATAREA                                                        
029400                                                                          
029500                                                                          
029600*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
029700*                                                                         
029800 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
029900*01  MID -COPY W4I41401                                                   
030000                                                                          
030100                                                                          
030200 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
030300*01  -COPY WMSGAREA                                                       
030400     03  MOD REDEFINES MSG-AREA.                                          
030500*      05  -COPY W4O41401                                                 
030600                                                                          
030700                                                                          
030800 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
030900*01  -COPY WMFSAREA                                                       
031000                                                                          
031100                                                                          
031200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
031300*                                                                         
031400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
031500                                                                          
031600 01  NYCKLAR-TILL-DLI.                                                    
031704                                                                          
031804     03 W-WDA5-MIN-X.                                                     
031904       05 W-IDDISTR-WDA5-MIN     PIC S9(5)   COMP-3.                      
032004       05 W-IDKUNDNR-WDA5-MIN    PIC S9(7)   COMP-3.                      
032104       05 FILLER                 PIC X(17)   VALUE LOW-VALUE.             
032204                                                                          
032304     03 W-WDA5-MAX-X.                                                     
032404       05 W-IDDISTR-WDA5-MAX     PIC S9(5)   COMP-3.                      
032504       05 W-IDKUNDNR-WDA5-MAX    PIC S9(7)   COMP-3.                      
032604       05 FILLER                 PIC X(17)   VALUE HIGH-VALUE.            
032700     03  W-IDGMT-X.                                                       
032800         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
032900         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
033004                                                                          
033104     03  W-WDB101KY-X.                                                    
033204         05  W-WDB1-IDPARTNR     PIC X(9)    VALUE SPACE.                 
033304         05  W-WDB1-IDFTG        PIC 9(2)    VALUE ZERO.                  
033400                                                                          
033504     03  W-WDB301KY-X.                                                    
033604         05  W-IDDC-WDB3         PIC X(2)    VALUE SPACE.                 
033704         05  W-IDDISTR-WDB3      PIC S9(5)   VALUE ZERO COMP-3.           
033804         05  W-IDKUNDNR-WDB3     PIC S9(7)   VALUE ZERO COMP-3.           
033904                                                                          
034004     03  W-WDB301KY-DEF-X.                                                
034104         05  W-IDDC-WDB3-DEF     PIC X(2)    VALUE SPACE.                 
034204         05  W-IDDISTR-WDB3-DEF  PIC S9(5)   VALUE ZERO COMP-3.           
034304         05  W-IDKUNDNR-WDB3-DEF PIC S9(7) VALUE +9999999 COMP-3.         
034404                                                                          
034500     03  W-IDDC-B6-X.                                                     
034600         05 W-IDDC-B6            PIC X(2).                                
034704                                                                          
034804     03  W-WDQ2CSEQ-MIN-X.                                                
034904         05  W-IDDISTR-Q2C-MIN   PIC S9(5)   VALUE ZERO COMP-3.           
035004         05  W-IDKUNDNR-Q2C-MIN  PIC S9(7)   VALUE ZERO COMP-3.           
035104         05  W-IDORDNR7-Q2C-MIN  PIC  9(7)   VALUE ZERO.                  
035200         05  FILLER          PIC  X(3)   VALUE SPACE.                     
035301                                                                          
035402     03  W-WDQ2CSEQ-MAX-X.                                                
035503         05  W-IDDISTR-Q2C-MAX   PIC S9(5)   VALUE ZERO COMP-3.           
035604         05  W-IDKUNDNR-Q2C-MAX  PIC S9(7)   VALUE ZERO COMP-3.           
035704         05  W-IDORDNR7-Q2C-MAX  PIC  9(7)   VALUE 9999999.               
035804         05  FILLER          PIC  X(3)   VALUE SPACE.                     
035904                                                                          
036004     03  W-IDDC-Q2-X.                                                     
036104         05 W-IDDC-Q2            PIC X(2).                                
036204                                                                          
036304     03  W-IDORDER-X.                                                     
036404         05 W-IDORDER            PIC S9(7)   COMP-3.                      
036504                                                                          
036604     03  W-KDORDSTA-SOK-X.                                                
036704         05 W-KDORDSTA-SOK       PIC X(1)    VALUE SPACE.                 
036804                                                                          
036900*                                                                         
037000*    --- STATUS-KOD FRÅN IMS                                              
037100 01  STATUS-WS                   PIC XX.                                  
037200     88  SEGMENT-FINNS                       VALUE '  '.                  
037300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
037404     88  SEGMENT-SLUT                        VALUE 'GB'.                  
037500                                                                          
037600 01  GODK-STATUSKODER.                                                    
037700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
037800                                                                          
037900 01  SSA1                        PIC X(121).                              
038004 01  SSA2                        PIC X(64).                               
038100                                                                          
038200                                                                          
038300*    --- IMS FUNKTIONSKODER                                               
038400*01  -COPY W0003                                                          
038500                                                                          
038600*    ---  DLI INPUT-OUTPUT AREA                                           
038700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
038804                                                                          
038904 01  FILLER               PIC X(16)   VALUE 'WDA501 AREA'.                
039004 01  DLI-IO-WDA501.                                                       
039104*    03  -COPY WDA501                                                     
039200                                                                          
039300 01  FILLER                      PIC X(16)   VALUE 'WDB101-AREA'.         
039400 01  DLI-IO-WDB101.                                                       
039500     03  WDB101.                                                          
039600*        05  -COPY WDB101  -PRE WDB1-                                     
039704                                                                          
039804 01  FILLER                      PIC X(16)   VALUE 'WDB201-AREA'.         
039904 01  DLI-IO-AREA.                                                         
040004     03  WDB201.                                                          
040104*        05  -COPY WDB201  -PRE WDB2-                                     
040200                                                                          
040304                                                                          
040404 01  FILLER                      PIC X(16)   VALUE 'WDB301-AREA'.         
040504 01  DLI-IO-WDB301.                                                       
040604     03  WDB301.                                                          
040704*        05  -COPY WDB301  -PRE WDB3-                                     
040804                                                                          
040900 01  FILLER                      PIC X(16)   VALUE 'WDB601 AREA'.         
041000 01   DLI-IO-AREA-B601.                                                   
041100*     03  -COPY WDB601                                                    
041204                                                                          
042100 01  FILLER                      PIC X(16)   VALUE 'WDQ201 AREA'.         
042101 01  DLI-IO-WDQ201.                                                       
042102*    03  -COPY WDQ201                                                     
042103                                                                          
042105 01  FILLER                      PIC X(16)   VALUE 'WDQ221 AREA'.         
042205 01  DLI-IO-WDQ221.                                                       
042305*    03  -COPY WDQ221                                                     
042405                                                                          
042505                                                                          
042605 LINKAGE SECTION.                                                         
042705*01  -COPY W0009   -PRE MSG-                                              
042805*01  -COPY W0008   -PRE WDP7-                                             
042905     05  FILLER                  PIC X.                                   
043005                                                                          
043105*01  -COPY W0008  -PRE WDA5-                                              
043205     05  FILLER                  PIC X.                                   
043305                                                                          
043405*01  -COPY W0008  -PRE WDB1-                                              
043505     05  FILLER                  PIC X.                                   
043605                                                                          
043705*01  -COPY W0008  -PRE WDB2-                                              
043805     05  FILLER                  PIC X.                                   
043905                                                                          
044005*01  -COPY W0008  -PRE WDB3-                                              
044105     05  FILLER                  PIC X.                                   
044205                                                                          
044305*01  -COPY W0008  -PRE WDB6-                                              
044405     05  FILLER                  PIC X.                                   
044505                                                                          
044605*01  -COPY W0008  -PRE WDQ2-                                              
044705     05  FILLER                  PIC X.                                   
044805                                                                          
044905 PROCEDURE DIVISION  USING MSG-PCB  WDP7-PCB                              
045005                           WDA5-PCB WDB1-PCB                              
045105                           WDB2-PCB WDB3-PCB WDB6-PCB                     
045205                           WDQ2-PCB.                                      
045305 MAIN SECTION.                                                            
045405     ENTRY 'DLITCBL' USING MSG-PCB  WDP7-PCB                              
045505                           WDA5-PCB WDB1-PCB                              
045605                           WDB2-PCB WDB3-PCB WDB6-PCB                     
045705                           WDQ2-PCB.                                      
045805                                                                          
045905     PERFORM IMS-GET-MSG                                                  
046005     IF SEGMENT-FINNS                                                     
046105       PERFORM A-INIT                                                     
046205       PERFORM B-KOLLA-NYCKLAR                                            
046305       IF NYCKLAR-OK                                                      
046405         IF MFS-UPDATE                                                    
046505           PERFORM C-KOLLA-INPUT                                          
046605           IF INDATA-OK                                                   
046705             PERFORM D-UPPDATERA                                          
046805           END-IF                                                         
046905         ELSE                                                             
047005           IF MFS-FIRST                                                   
047105              PERFORM E-FOERSTA-SIDA                                      
047205           ELSE                                                           
047305              PERFORM F-SAMMA-SIDA                                        
047405           END-IF                                                         
047505         END-IF                                                           
047605         IF INDATA-OK AND ALLT-OK                                         
047705           PERFORM G-LAES-VISA-INFO                                       
047805         END-IF                                                           
047905       END-IF                                                             
048005*    --- IF ANSWER TO SCREEN:       MSG-KVLL = MOD-LENGTH + 4             
048105*    --- IF PROGRAM-TO-PROGRAM-SWITCH:       = MOD-LENGTH + 17            
048205       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O41401 + 4                      
048305*      CALL FELLOG                                                        
048405       PERFORM IMS-INSERT-MSG                                             
048505     END-IF                                                               
048605                                                                          
048705     MOVE ZERO TO RETURN-CODE                                             
048805     GOBACK                                                               
048905     .                                                                    
049005                                                                          
049105 A-INIT SECTION.                                                          
049205     MOVE 'A-INIT          ' TO CURR-SECTION.                             
049305                                                                          
049405     IF MSG-DUBBLA-TRANSKODER                                             
049505       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I41401                 
049605       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
049705       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
049805     ELSE                                                                 
049905       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I41401                  
050005       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
050105       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
050205     END-IF                                                               
050305                                                                          
050405     MOVE MSG-KDTRTYP      TO MFS-KDTRTYP                                 
050505     MOVE MSG-IDPFK        TO MFS-IDPFK                                   
050605     MOVE MFS-IDTRANS      TO W-IDTRANS                                   
050705                                                                          
050805     MOVE LOW-VALUE        TO MSG-AREA                                    
050905     MOVE 'W4O414N1'       TO MFS-IDMOD                                   
051005     MOVE '4414'           TO MOD-IDTRANS                                 
051105     MOVE MFS-RENSA-FAELT  TO MOD-TEMFSFEL MOD-TEMFSINF                   
051205                                                                          
051305     IF EGEN-MID OR HELP-MID                                              
051405       CONTINUE                                                           
051505     ELSE                                                                 
051605       MOVE SPACE          TO MFS-KDTRTYP                                 
051705       MOVE '7'            TO MFS-IDPFK                                   
051805     END-IF                                                               
051905                                                                          
052005     MOVE 'GB'             TO MED-IDSKYLT                                 
052105     ACCEPT DAGENS-DATUM FROM DATE                                        
052205     .                                                                    
052305                                                                          
052405 B-KOLLA-NYCKLAR SECTION.                                                 
052505     MOVE 'B-KOLLA-NYCKLAR ' TO CURR-SECTION.                             
052605                                                                          
052705     MOVE ALL '+'           TO MSGI-WMSGINIT                              
052805     MOVE '001'             TO MSGI-KDCALL                                
052905     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
053005     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
053105     MOVE '4414'            TO MSGI-IDTRANS                               
053205     IF EGEN-MID                                                          
053305       MOVE MID-IDDISTR-IN     TO MSGI-IDDISTR                            
053405       MOVE MID-IDKUNDNR-IN    TO MSGI-IDKUNDNR                           
053505     END-IF                                                               
053605     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
053705     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
053805                                                                          
053905     MOVE JA TO NYCKLAR-SW                                                
054005     MOVE JA TO INDATA-SW                                                 
054105                                                                          
054205                                                                          
054305*    -- KONTROLL AV IDDISTR                                               
054405     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                               
054505                                                                          
054605     IF MID-IDDISTR-IN NOT = ALL '+'                                      
054705       MOVE '7'         TO MFS-IDPFK                                      
054805       MOVE SPACE       TO MFS-KDTRTYP                                    
054905     END-IF                                                               
055005     INSPECT MSGI-IDDISTR REPLACING LEADING SPACE BY ZERO                 
055105     IF MSGI-IDDISTR NUMERIC                                              
055205       MOVE MSGI-IDDISTR TO W-IDDISTR                                     
055305                            W-IDDISTR-WDB3                                
055405                            W-IDDISTR-WDB3-DEF                            
055505                            W-IDDISTR-WDA5-MIN                            
055605                            W-IDDISTR-WDA5-MAX                            
055705                            W-IDDISTR-Q2C-MIN                             
055805                            W-IDDISTR-Q2C-MAX                             
055905     ELSE                                                                 
056005       MOVE NEJ TO NYCKLAR-SW                                             
056105     END-IF                                                               
056205                                                                          
056305*    -- KONTROLL AV IDKUNDNR                                              
056405     MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR-IN                              
056505                                                                          
056605     IF MID-IDKUNDNR-IN NOT = ALL '+'                                     
056705       MOVE '7'         TO MFS-IDPFK                                      
056805       MOVE SPACE       TO MFS-KDTRTYP                                    
056905     END-IF                                                               
057005     INSPECT MSGI-IDKUNDNR REPLACING LEADING SPACE BY ZERO                
057105     IF MSGI-IDKUNDNR NUMERIC                                             
057205       MOVE MSGI-IDKUNDNR TO W-IDKUNDNR                                   
057305                             W-IDKUNDNR-WDB3                              
057405                             W-IDKUNDNR-WDA5-MIN                          
057505                             W-IDKUNDNR-WDA5-MAX                          
057605                             W-IDKUNDNR-Q2C-MIN                           
057705                             W-IDKUNDNR-Q2C-MAX                           
057805     ELSE                                                                 
057905       MOVE NEJ TO NYCKLAR-SW                                             
058005     END-IF                                                               
058105                                                                          
058205     IF GODK-MID OR NYCKLAR-OK                                            
058305       MOVE MSGI-IDDISTR        TO MOD-IDDISTR-UT                         
058405       INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE             
058505       MOVE MSGI-IDKUNDNR       TO MOD-IDKUNDNR-UT                        
058605       INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE            
058705     ELSE                                                                 
058805       MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-UT                             
058905                               MOD-IDKUNDNR-UT                            
059005     END-IF                                                               
059105                                                                          
059205     IF NYCKLAR-FEL                                                       
059305       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
059405       CALL WMEDKONV USING MED-WMEDAREA                                   
059505       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
059605       PERFORM MFS-RENSA-FAELT-IN                                         
059705       PERFORM MFS-RENSA-FAELT-UT                                         
059805     END-IF                                                               
059905     .                                                                    
060005                                                                          
060105 C-KOLLA-INPUT SECTION.                                                   
060205     MOVE 'C-KOLLA-INPUT   ' TO CURR-SECTION.                             
060305                                                                          
060405     MOVE JA    TO INDATA-SW                                              
060505     MOVE ZERO  TO WS-TISTADAT                                            
060605                   WS-TISTODAT                                            
060705                                                                          
060805     MOVE SPACE TO MED-IDMFSFEL                                           
060905     PERFORM IMS-GU-WDB201                                                
061005                                                                          
061105     IF SEGMENT-SAKNAS                                                    
061205        MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                
061305        PERFORM MFS-RENSA-FAELT-UT                                        
061405        PERFORM MFS-RENSA-FAELT-IN                                        
061505        MOVE NEJ TO INDATA-SW                                             
061605     ELSE                                                                 
061705        IF MID-INPUT = ALL '+'                                            
061805           MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                      
061905           MOVE NEJ TO INDATA-SW                                          
063500        END-IF                                                            
063600     END-IF                                                               
063700                                                                          
063800     IF INDATA-OK                                                         
063900        PERFORM CA-KOLLA-DC-BULK                                          
064000        PERFORM CB-KOLLA-DC-DAY                                           
064100        PERFORM CC-KOLLA-DC-VOR                                           
065904     END-IF                                                               
066000                                                                          
066104     IF INDATA-OK                                                         
066204        PERFORM CD-KOLLA-START-STOP                                       
066304     END-IF                                                               
066404     IF INDATA-OK                                                         
066504        PERFORM CE-KOLLA-DC-DAY-ALT                                       
066604     END-IF                                                               
066700     IF INDATA-FEL                                                        
066800        IF MED-IDMFSFEL = SPACE                                           
066900           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
067000        END-IF                                                            
067100        CALL WMEDKONV USING MED-WMEDAREA                                  
067200        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
067300        PERFORM MFS-ROER-EJ-FAELT-IN                                      
067400        PERFORM MFS-ROER-EJ-FAELT-UT                                      
067500     END-IF                                                               
067600     .                                                                    
067700                                                                          
067800 CA-KOLLA-DC-BULK SECTION.                                                
067904     MOVE 'CA-KOLLA-DC-BULK' TO CURR-SECTION.                             
068000                                                                          
068100     MOVE +1    TO INDX                                                   
068200     MOVE +1    TO INDX-DC                                                
068300     MOVE SPACE TO DCS-IDDC                                               
068400                                                                          
068500     MOVE SPACE TO W-IDDC-HELP-ORDN                                       
068600                                                                          
068700     PERFORM UNTIL INDX > MAX-INDX                                        
068800         IF MID-IDDC-BULK (INDX) = ALL '+' OR SPACE                       
068900           CONTINUE                                                       
069000         ELSE                                                             
069100           MOVE MID-IDDC-BULK (INDX) TO W-IDDC-WDB3                       
069204                                        W-IDDC-WDB3-DEF                   
069304                                                                          
069404           IF DCS-IDDC NOT = MID-IDDC-BULK (INDX)                         
069504              MOVE MID-IDDC-BULK (INDX) TO W-IDDC-B6                      
069604              PERFORM IMS-GU-WDB601                                       
069704           END-IF                                                         
069804                                                                          
069904           IF DCS-KDDC = SPACE OR DCS-DDC OR DCS-CDC-TR                   
070004             MOVE MFS-ALFA-FAELT-FEL TO MOD-IDDC-BULK-ATTR (INDX)         
070104             MOVE NEJ TO INDATA-SW                                        
070204           ELSE                                                           
070304             PERFORM IMS-GU-WDB301                                        
070404             IF SEGMENT-FINNS                                             
070504               IF WDB3-DC-KDGENFRA-MO = +0                                
070604                 MOVE MFS-ALFA-FAELT-FEL TO                               
070704                      MOD-IDDC-BULK-ATTR (INDX)                           
070800                 MOVE NEJ TO INDATA-SW                                    
070901               ELSE                                                       
071002                 MOVE MFS-ALFA-FAELT-RAETT TO                             
071103                      MOD-IDDC-BULK-ATTR(INDX)                            
071204               END-IF                                                     
071304             ELSE                                                         
071404               MOVE MFS-ALFA-FAELT-FEL TO                                 
071504                    MOD-IDDC-BULK-ATTR(INDX)                              
071604               MOVE NEJ TO INDATA-SW                                      
071704             END-IF                                                       
071804           END-IF                                                         
071904         END-IF                                                           
072004       ADD +1 TO INDX                                                     
072104     END-PERFORM                                                          
072204                                                                          
072304     MOVE W-IDDISTR TO TEST-IDDISTR                                       
072404     IF DIS108-SPEC                                                       
072504        IF DIS108-KINA                                                    
072604           SET BULK-KL TO TRUE                                            
072704           PERFORM S01-KOLLA-KINA-DC                                      
072804        END-IF                                                            
072904     ELSE                                                                 
073004        PERFORM CAB-KOLLA-DC11-SIST                                       
073104     END-IF                                                               
073204                                                                          
073304     IF INDATA-OK                                                         
073404       PERFORM CAC-KOLLA-OLD-DC-BULK                                      
073504     END-IF                                                               
073604                                                                          
073704     IF INDATA-OK                                                         
073804        SET BULK-KL TO TRUE                                               
073904        PERFORM CAA-JUSTERA-MID                                           
074004        PERFORM S02-KOLLA-DUBLETTER                                       
074104        PERFORM S03-KOLLA-BLANDNING                                       
074204        PERFORM S07-KOLLA-MAX-ANTAL                                       
074304     END-IF                                                               
074404                                                                          
074504     .                                                                    
074604     EJECT                                                                
074704                                                                          
074804 CAA-JUSTERA-MID     SECTION.                                             
074904                                                                          
075004     MOVE +1    TO INDX                                                   
075104     MOVE +1    TO INDX-DC                                                
075204                                                                          
075304                                                                          
075404     PERFORM UNTIL INDX > MAX-INDX                                        
075504         IF MID-IDDC-BULK (INDX) = ALL '+' OR SPACE                       
075604            CONTINUE                                                      
075704         ELSE                                                             
075804           MOVE MID-IDDC-BULK (INDX) TO                                   
075904                MID-IDDC-BULK (INDX-DC)                                   
076004                                                                          
076104           ADD +1 TO INDX-DC                                              
076204         END-IF                                                           
076304         ADD +1 TO INDX                                                   
076404     END-PERFORM                                                          
076504                                                                          
076604     PERFORM UNTIL INDX-DC > MAX-INDX                                     
076704        MOVE SPACE TO MID-IDDC-BULK (INDX-DC)                             
076804        ADD +1 TO INDX-DC                                                 
076904     END-PERFORM                                                          
077004*    NU HAR VI EN SNYGG OCH PRYDLIG MID                                   
077104     .                                                                    
077204     EJECT                                                                
077304                                                                          
077404 CAB-KOLLA-DC11-SIST SECTION.                                             
077504                                                                          
077604     MOVE +1    TO INDX                                                   
077704     MOVE +1    TO SISTA-INDX                                             
077804     MOVE SPACE TO W-SISTA-DC                                             
077904     MOVE SPACE TO W-NAST-SISTA-DC                                        
078004     PERFORM UNTIL INDX > MAX-INDX                                        
078104       IF MID-IDDC-BULK (INDX) = ALL '+' OR SPACE                         
078204          CONTINUE                                                        
078304       ELSE                                                               
078404          MOVE W-SISTA-DC           TO W-NAST-SISTA-DC                    
078504          MOVE MID-IDDC-BULK (INDX) TO W-SISTA-DC                         
078604          MOVE INDX TO SISTA-INDX                                         
078704       END-IF                                                             
078804       ADD +1 TO INDX                                                     
078904     END-PERFORM                                                          
079004     IF W-SISTA-DC NOT = WC-CDC-SE AND                                    
079104        W-NAST-SISTA-DC NOT = WC-CDC-SE                                   
079204        MOVE MFS-ALFA-FAELT-FEL                                           
079304                 TO   MOD-IDDC-BULK-ATTR (SISTA-INDX)                     
079404        MOVE NEJ TO INDATA-SW                                             
079504     END-IF                                                               
079604     .                                                                    
079704     EJECT                                                                
079804                                                                          
079904 CAC-KOLLA-OLD-DC-BULK SECTION.                                           
080004* CHECKS IF THE DC IN THE DC STEERING TABLE HAS BEEN DELETED OR           
080104* REPLACED. IF YES, CHECKS IF ANY BACKORDERS EXIST ON WDA5 FOR            
080204* THE DISTRICT/CUSTOMER/DC COMBINATION. IF BACKORDERS EXIST, THE          
080304* DC CANNOT BE CHANGED/DELETED FROM THE STEERING TABLE.                   
080400                                                                          
080501                                                                          
080602* CHECK IF DC IS SHUFFLED AND CHANGED/DELETED ON SCREEN                   
080703     MOVE +1 TO INDX                                                      
080804                INDX1                                                     
080904     PERFORM UNTIL INDX > MAX-INDX                                        
081004       PERFORM UNTIL INDX1 > MAX-INDX OR DC-FOUND                         
081104*   COMPARE MID WITH DB VALUE.                                            
081204          IF WDB2-GMT-IDDC-BULK(INDX) = MID-IDDC-BULK(INDX1)              
081304             MOVE JA                          TO DC-FOUND-SW              
081404             MOVE SPACES                      TO WS-BULK-DC-NEW           
081504          ELSE                                                            
081604             IF WDB2-GMT-IDDC-BULK(INDX) NOT = SPACES                     
081704                MOVE WDB2-GMT-IDDC-BULK(INDX) TO WS-BULK-DC-NEW           
081804             END-IF                                                       
081904          END-IF                                                          
082004          ADD +1 TO INDX1                                                 
082104       END-PERFORM                                                        
082204* IF THE MID DC HAS BEEN REPLACED/DELETED,                                
082304* CHECK FOR BACKORDERS ON THE DELETED/REPLACED DC.                        
082404       IF WS-BULK-DC-NEW NOT = SPACES                                     
082504          MOVE WS-BULK-DC-NEW                 TO W-WDA5-IDDC              
082604                                                 WS-IDDC-WORK             
082704          PERFORM S06-CHECK-WDA5                                          
082804          IF WS-DEL-DC = JA                                               
082904             MOVE 2                           TO W-KDORDKL-MIN            
083004             MOVE 4                           TO W-KDORDKL-MAX            
083104             PERFORM S08-CHECK-WDQ2                                       
083204          END-IF                                                          
083304          IF WS-DEL-DC = NEJ                                              
083404             MOVE MFS-ALFA-FAELT-FEL          TO                          
083504                             MOD-IDDC-BULK-ATTR (INDX)                    
083604             MOVE NEJ                         TO INDATA-SW                
083704          END-IF                                                          
083804       END-IF                                                             
083904       ADD +1                                 TO INDX                     
084004                                                                          
084104       MOVE NEJ                               TO DC-FOUND-SW              
084204       MOVE +1                                TO INDX1                    
084304     END-PERFORM                                                          
084404     .                                                                    
084504     EJECT                                                                
084604 CB-KOLLA-DC-DAY SECTION.                                                 
084704                                                                          
084804     MOVE +1    TO INDX                                                   
084904     MOVE +1    TO INDX-DC                                                
085004     MOVE SPACE TO DCS-IDDC                                               
085104                                                                          
085204     MOVE SPACE TO W-IDDC-HELP-ORDN                                       
085304                                                                          
085404     PERFORM UNTIL INDX > MAX-INDX                                        
085504         IF MID-IDDC-DAY (INDX) = ALL '+' OR SPACE                        
085604           CONTINUE                                                       
085704         ELSE                                                             
085804           MOVE MID-IDDC-DAY (INDX) TO W-IDDC-WDB3                        
085904                                       W-IDDC-WDB3-DEF                    
086004                                                                          
086104           IF DCS-IDDC NOT = MID-IDDC-DAY (INDX)                          
086204              MOVE MID-IDDC-DAY (INDX) TO W-IDDC-B6                       
086304              PERFORM IMS-GU-WDB601                                       
086404           END-IF                                                         
086504           IF DCS-KDDC = SPACE OR DCS-DDC OR DCS-CDC-TR                   
086604             MOVE MFS-ALFA-FAELT-FEL TO MOD-IDDC-DAY-ATTR (INDX)          
086704             MOVE NEJ TO INDATA-SW                                        
086804           ELSE                                                           
086904             PERFORM IMS-GU-WDB301                                        
087004             IF SEGMENT-FINNS                                             
087104               IF WDB3-DC-KDGENFRA-DO = +0                                
087204                 MOVE MFS-ALFA-FAELT-FEL TO                               
087304                      MOD-IDDC-DAY-ATTR(INDX)                             
087404                 MOVE NEJ TO INDATA-SW                                    
087504               ELSE                                                       
087604                 MOVE MFS-ALFA-FAELT-RAETT TO                             
087704                      MOD-IDDC-DAY-ATTR (INDX)                            
087804                 MOVE MID-IDDC-DAY  (INDX) TO                             
087904                      W-IDDC-ORDN (INDX-DC)                               
088004                 ADD +1 TO INDX-DC                                        
088104               END-IF                                                     
088204             ELSE                                                         
088304               MOVE MFS-ALFA-FAELT-FEL TO                                 
088404                    MOD-IDDC-DAY-ATTR(INDX)                               
088504               MOVE NEJ TO INDATA-SW                                      
088604             END-IF                                                       
088704           END-IF                                                         
088804         END-IF                                                           
088904       ADD +1 TO INDX                                                     
089004     END-PERFORM                                                          
089104                                                                          
089204     MOVE W-IDDISTR TO TEST-IDDISTR                                       
089304     IF DIS108-SPEC                                                       
089404        IF DIS108-KINA                                                    
089504           SET DAY-KL  TO TRUE                                            
089604           PERFORM S01-KOLLA-KINA-DC                                      
089704        END-IF                                                            
089804     ELSE                                                                 
089904        PERFORM CBB-KOLLA-DC11-SIST                                       
090004     END-IF                                                               
090104                                                                          
090204     IF INDATA-OK                                                         
090304       PERFORM CBC-KOLLA-OLD-DC-DAY                                       
090400     END-IF                                                               
090501                                                                          
090602     IF INDATA-OK                                                         
090703        SET DAY-KL TO TRUE                                                
090804        PERFORM CBA-JUSTERA-MID                                           
090904        PERFORM S02-KOLLA-DUBLETTER                                       
091004        PERFORM S03-KOLLA-BLANDNING                                       
091104        PERFORM S07-KOLLA-MAX-ANTAL                                       
091204     END-IF                                                               
091304     .                                                                    
091404     EJECT                                                                
091504                                                                          
091604 CBA-JUSTERA-MID     SECTION.                                             
091704                                                                          
091804     MOVE +1    TO INDX                                                   
091904     MOVE +1    TO INDX-DC                                                
092004                                                                          
092104                                                                          
092204     PERFORM UNTIL INDX > MAX-INDX                                        
092304         IF MID-IDDC-DAY (INDX) = ALL '+' OR SPACE                        
092404            CONTINUE                                                      
092504         ELSE                                                             
092604           MOVE MID-IDDC-DAY (INDX) TO                                    
092704                MID-IDDC-DAY (INDX-DC)                                    
092804                                                                          
092904           ADD +1 TO INDX-DC                                              
093004         END-IF                                                           
093104         ADD +1 TO INDX                                                   
093204     END-PERFORM                                                          
093304                                                                          
093404     PERFORM UNTIL INDX-DC > MAX-INDX                                     
093504        MOVE SPACE TO MID-IDDC-DAY (INDX-DC)                              
093604        ADD +1 TO INDX-DC                                                 
093704     END-PERFORM                                                          
093804*    NU HAR VI EN SNYGG OCH PRYDLIG MID                                   
093904     .                                                                    
094004     EJECT                                                                
094104                                                                          
094204 CBB-KOLLA-DC11-SIST SECTION.                                             
094304                                                                          
094404     MOVE +1    TO INDX                                                   
094504     MOVE +1    TO SISTA-INDX                                             
094604     MOVE SPACE TO W-SISTA-DC                                             
094704     MOVE SPACE TO W-NAST-SISTA-DC                                        
094804     PERFORM UNTIL INDX > MAX-INDX                                        
094904       IF MID-IDDC-DAY  (INDX) = ALL '+' OR SPACE                         
095004          CONTINUE                                                        
095104       ELSE                                                               
095204          MOVE W-SISTA-DC           TO W-NAST-SISTA-DC                    
095304          MOVE MID-IDDC-DAY  (INDX) TO W-SISTA-DC                         
095404          MOVE INDX TO SISTA-INDX                                         
095504       END-IF                                                             
095604       ADD +1 TO INDX                                                     
095704     END-PERFORM                                                          
095804     IF W-SISTA-DC NOT = WC-CDC-SE AND                                    
095904        W-NAST-SISTA-DC NOT = WC-CDC-SE                                   
096004        MOVE MFS-ALFA-FAELT-FEL                                           
096104                 TO   MOD-IDDC-DAY-ATTR (SISTA-INDX)                      
096204        MOVE NEJ TO INDATA-SW                                             
096304     END-IF                                                               
096404     .                                                                    
096504     EJECT                                                                
096604                                                                          
096704 CBC-KOLLA-OLD-DC-DAY SECTION.                                            
096804* CHECKS IF THE DC IN THE DC STEERING TABLE HAS BEEN DELETED OR           
096904* REPLACED. IF YES, CHECKS IF ANY BACKORDERS EXIST ON WDA5 FOR            
097004* THE DISTRICT/CUSTOMER/DC COMBINATION. IF BACKORDERS EXIST, THE          
097104* DC CANNOT BE CHANGED/DELETED FROM THE STEERING TABLE.                   
097204                                                                          
097304                                                                          
097404* CHECK IF DC IS SHUFFLED AND CHANGED/DELETED ON SCREEN                   
097504     MOVE SPACES  TO WS-DAY-DC-NEW                                        
097604                     WS-DEL-DC                                            
097704                     DC-FOUND-SW                                          
097804     MOVE +1 TO INDX                                                      
097904                INDX1                                                     
098004     PERFORM UNTIL INDX > MAX-INDX                                        
098104       PERFORM UNTIL INDX1 > MAX-INDX OR DC-FOUND                         
098204*   COMPARE MID WITH DB VALUE.                                            
098304          IF WDB2-GMT-IDDC-DAY(INDX) = MID-IDDC-DAY(INDX1)                
098404             MOVE JA                          TO DC-FOUND-SW              
098504             MOVE SPACES                      TO WS-DAY-DC-NEW            
098604          ELSE                                                            
098704             IF WDB2-GMT-IDDC-DAY(INDX) NOT = SPACES                      
098804                MOVE WDB2-GMT-IDDC-DAY(INDX)  TO WS-DAY-DC-NEW            
098904             END-IF                                                       
099004          END-IF                                                          
099104          ADD +1 TO INDX1                                                 
099204       END-PERFORM                                                        
099304* IF THE MID DC HAS BEEN REPLACED/DELETED,                                
099404* CHECK FOR BACKORDERS ON THE DELETED/REPLACED DC.                        
099504       IF WS-DAY-DC-NEW NOT = SPACES                                      
099604          MOVE WS-DAY-DC-NEW                  TO W-WDA5-IDDC              
099704                                                 WS-IDDC-WORK             
099804          PERFORM S06-CHECK-WDA5                                          
099904          IF WS-DEL-DC = JA                                               
100004            MOVE 1                           TO W-KDORDKL-MIN             
100104                                                W-KDORDKL-MAX             
100204            PERFORM S08-CHECK-WDQ2                                        
100300          END-IF                                                          
100401          IF WS-DEL-DC = NEJ                                              
100502             MOVE MFS-ALFA-FAELT-FEL          TO                          
100603                             MOD-IDDC-DAY-ATTR (INDX)                     
100704             MOVE NEJ                         TO INDATA-SW                
100804          END-IF                                                          
100904       END-IF                                                             
101004       ADD +1                                 TO INDX                     
101104                                                                          
101204       MOVE NEJ                               TO DC-FOUND-SW              
101304       MOVE +1                                TO INDX1                    
101404     END-PERFORM                                                          
101504     .                                                                    
101604     EJECT                                                                
101704 CC-KOLLA-DC-VOR  SECTION.                                                
101804                                                                          
101904     MOVE +1    TO INDX                                                   
102004     MOVE +1    TO INDX-DC                                                
102104     MOVE SPACE TO DCS-IDDC                                               
102204                                                                          
102304     MOVE SPACE TO W-IDDC-HELP-ORDN                                       
102404                                                                          
102504     PERFORM UNTIL INDX > MAX-INDX                                        
102604         IF MID-IDDC-VOR (INDX) = ALL '+' OR SPACE                        
102704           CONTINUE                                                       
102804         ELSE                                                             
102904           MOVE MID-IDDC-VOR (INDX) TO W-IDDC-WDB3                        
103004                                       W-IDDC-WDB3-DEF                    
103104                                                                          
103204           IF DCS-IDDC NOT = MID-IDDC-VOR (INDX)                          
103304              MOVE MID-IDDC-VOR (INDX) TO W-IDDC-B6                       
103404              PERFORM IMS-GU-WDB601                                       
103504           END-IF                                                         
103604           IF DCS-KDDC = SPACE OR DCS-DDC                                 
103704             MOVE MFS-ALFA-FAELT-FEL TO MOD-IDDC-VOR-ATTR (INDX)          
103804             MOVE NEJ TO INDATA-SW                                        
103904           ELSE                                                           
104004             PERFORM IMS-GU-WDB301                                        
104104             IF SEGMENT-FINNS                                             
104204               IF WDB3-DC-KDGENFRA-VOR = +0                               
104304                 MOVE MFS-ALFA-FAELT-FEL TO                               
104404                      MOD-IDDC-VOR-ATTR(INDX)                             
104504                 MOVE NEJ TO INDATA-SW                                    
104604               ELSE                                                       
104704                 MOVE MFS-ALFA-FAELT-RAETT TO                             
104804                      MOD-IDDC-VOR-ATTR (INDX)                            
104904                 MOVE MID-IDDC-VOR  (INDX) TO                             
105004                      W-IDDC-ORDN (INDX-DC)                               
105104                 ADD +1 TO INDX-DC                                        
105204               END-IF                                                     
105304             ELSE                                                         
105404               MOVE MFS-ALFA-FAELT-FEL TO                                 
105504                    MOD-IDDC-VOR-ATTR(INDX)                               
105604               MOVE NEJ TO INDATA-SW                                      
105704             END-IF                                                       
105804           END-IF                                                         
105904         END-IF                                                           
106004       ADD +1 TO INDX                                                     
106104     END-PERFORM                                                          
106204                                                                          
106304     MOVE W-IDDISTR TO TEST-IDDISTR                                       
106404     IF DIS108-SPEC                                                       
106504        IF DIS108-KINA                                                    
106604           SET VOR-KL  TO TRUE                                            
106704           PERFORM S01-KOLLA-KINA-DC                                      
106804        END-IF                                                            
106904     ELSE                                                                 
107004        PERFORM CCB-KOLLA-DC11-SIST                                       
107104     END-IF                                                               
107204                                                                          
107304     IF INDATA-OK                                                         
107404       PERFORM CCC-KOLLA-OLD-DC-VOR                                       
107504     END-IF                                                               
107604                                                                          
107704     IF INDATA-OK                                                         
107804        SET VOR-KL TO TRUE                                                
107904        PERFORM CCA-JUSTERA-MID                                           
108004        PERFORM S02-KOLLA-DUBLETTER                                       
108104        PERFORM S03-KOLLA-BLANDNING                                       
108204        PERFORM S07-KOLLA-MAX-ANTAL                                       
108304     END-IF                                                               
108404     .                                                                    
108504     EJECT                                                                
108604                                                                          
108704 CCA-JUSTERA-MID     SECTION.                                             
108804                                                                          
108904     MOVE +1    TO INDX                                                   
109004     MOVE +1    TO INDX-DC                                                
109104                                                                          
109204                                                                          
109304     PERFORM UNTIL INDX > MAX-INDX                                        
109404         IF MID-IDDC-VOR (INDX) = ALL '+' OR SPACE                        
109504            CONTINUE                                                      
109600         ELSE                                                             
109701           MOVE MID-IDDC-VOR (INDX) TO                                    
109802                MID-IDDC-VOR (INDX-DC)                                    
109903                                                                          
110004           ADD +1 TO INDX-DC                                              
110104         END-IF                                                           
110204         ADD +1 TO INDX                                                   
110304     END-PERFORM                                                          
110404                                                                          
110504     PERFORM UNTIL INDX-DC > MAX-INDX                                     
110604        MOVE SPACE TO MID-IDDC-VOR (INDX-DC)                              
110704        ADD +1 TO INDX-DC                                                 
110804     END-PERFORM                                                          
110904*    NU HAR VI EN SNYGG OCH PRYDLIG MID                                   
111004     .                                                                    
111104     EJECT                                                                
111204                                                                          
111304 CCB-KOLLA-DC11-SIST SECTION.                                             
111404                                                                          
111504     MOVE +1    TO INDX                                                   
111604     MOVE +1    TO SISTA-INDX                                             
111704     MOVE SPACE TO W-SISTA-DC                                             
111804     MOVE SPACE TO W-NAST-SISTA-DC                                        
111904     PERFORM UNTIL INDX > MAX-INDX                                        
112004       IF MID-IDDC-VOR  (INDX) = ALL '+' OR SPACE                         
112104          CONTINUE                                                        
112204       ELSE                                                               
112304          MOVE W-SISTA-DC           TO W-NAST-SISTA-DC                    
112404          MOVE MID-IDDC-VOR  (INDX) TO W-SISTA-DC                         
112504          MOVE INDX TO SISTA-INDX                                         
112604       END-IF                                                             
112704       ADD +1 TO INDX                                                     
112804     END-PERFORM                                                          
112904     IF W-SISTA-DC NOT = WC-CDC-SE AND                                    
113004        W-NAST-SISTA-DC NOT = WC-CDC-SE                                   
113104        MOVE MFS-ALFA-FAELT-FEL                                           
113204                 TO   MOD-IDDC-VOR-ATTR (SISTA-INDX)                      
113304        MOVE NEJ TO INDATA-SW                                             
113404     END-IF                                                               
113504     .                                                                    
113604     EJECT                                                                
113704                                                                          
113804 CCC-KOLLA-OLD-DC-VOR SECTION.                                            
113904* CHECKS IF THE DC IN THE DC STEERING TABLE HAS BEEN DELETED OR           
114004* REPLACED. IF YES, CHECKS IF ANY BACKORDERS EXIST ON WDA5 FOR            
114104* THE DISTRICT/CUSTOMER/DC COMBINATION. IF BACKORDERS EXIST, THE          
114204* DC CANNOT BE CHANGED/DELETED FROM THE STEERING TABLE.                   
114304                                                                          
114404                                                                          
114504* CHECK IF DC IS SHUFFLED AND CHANGED/DELETED ON SCREEN                   
114604     MOVE SPACES  TO WS-VOR-DC-NEW                                        
114704                     WS-DEL-DC                                            
114804                     DC-FOUND-SW                                          
114904     MOVE +1 TO INDX                                                      
115004                INDX1                                                     
115104     PERFORM UNTIL INDX > MAX-INDX                                        
115204       PERFORM UNTIL INDX1 > MAX-INDX OR DC-FOUND                         
115304*   COMPARE MID WITH DB VALUE.                                            
115404          IF WDB2-GMT-IDDC-VOR(INDX) = MID-IDDC-VOR(INDX1)                
115504             MOVE JA                          TO DC-FOUND-SW              
115604             MOVE SPACES                      TO WS-VOR-DC-NEW            
115704          ELSE                                                            
115804             IF WDB2-GMT-IDDC-VOR(INDX) NOT = SPACES                      
115904                MOVE WDB2-GMT-IDDC-VOR(INDX) TO WS-VOR-DC-NEW             
116004             END-IF                                                       
116104          END-IF                                                          
116204          ADD +1 TO INDX1                                                 
116304       END-PERFORM                                                        
116404* IF THE MID DC HAS BEEN REPLACED/DELETED,                                
116504* CHECK FOR BACKORDERS ON THE DELETED/REPLACED DC.                        
116604       IF WS-VOR-DC-NEW NOT = SPACES                                      
116704          MOVE WS-VOR-DC-NEW                 TO WS-IDDC-WORK              
116804                                                                          
116904          MOVE 0                             TO W-KDORDKL-MIN             
117004                                                W-KDORDKL-MAX             
117104          PERFORM S08-CHECK-WDQ2                                          
117204          IF WS-DEL-DC = NEJ                                              
117304             MOVE MFS-ALFA-FAELT-FEL          TO                          
117404                             MOD-IDDC-VOR-ATTR (INDX)                     
117504             MOVE NEJ                         TO INDATA-SW                
117604          END-IF                                                          
117704       END-IF                                                             
117804       ADD +1                                 TO INDX                     
117904                                                                          
118004       MOVE NEJ                               TO DC-FOUND-SW              
118104       MOVE +1                                TO INDX1                    
118204     END-PERFORM                                                          
118304     .                                                                    
118404                                                                          
118504                                                                          
118604 CD-KOLLA-START-STOP SECTION.                                             
118704                                                                          
118804     IF MID-KVDAGAR-DOW NOT = ALL '+'                                     
118904       IF MID-KVDAGAR-DOW NUMERIC                                         
119004         MOVE MFS-NUM-FAELT-RAETT TO MOD-KVDAGAR-DOW-ATTR                 
119104       ELSE                                                               
119204         MOVE MFS-NUM-FAELT-FEL   TO MOD-KVDAGAR-DOW-ATTR                 
119304         MOVE NEJ TO INDATA-SW                                            
119404       END-IF                                                             
119504     END-IF                                                               
119604                                                                          
119704     IF MID-TISTADAT NOT = ALL '+'                                        
119804        MOVE MID-TISTADAT TO DAT-I-TIDATUM                                
119904        MOVE 'AAMMDD'  TO DAT-KDDATFORM                                   
120004                                                                          
120104        CALL WDATKONV USING DAT-KDDATFORM                                 
120204                            DAT-I-TIDATUM                                 
120304                            DAT-O-TIDATUM                                 
120404                            DAT-KDSVAR                                    
120504                                                                          
120604        IF DAT-KDSVAR-OK                                                  
120704           MOVE DAT-TIAAMMDD TO WS-TISTADAT                               
120804           IF WS-TISTADAT >= DAGENS-DATUM                                 
120904              MOVE MFS-NUM-FAELT-RAETT TO MOD-TISTADAT-ATTR               
121004           ELSE                                                           
121104              MOVE MFS-NUM-FAELT-FEL TO MOD-TISTADAT-ATTR                 
121204              MOVE NEJ TO INDATA-SW                                       
121304           END-IF                                                         
121404        ELSE                                                              
121504           MOVE MFS-NUM-FAELT-FEL TO MOD-TISTADAT-ATTR                    
121604           MOVE NEJ TO INDATA-SW                                          
121704        END-IF                                                            
121804        MOVE WDB2-GMT-IDPARTNR TO W-WDB1-IDPARTNR                         
121904        MOVE WDB2-GMT-IDFTG    TO W-WDB1-IDFTG                            
122004        PERFORM IMS-GU-WDB101                                             
122104        IF SEGMENT-FINNS AND (WDB1-BET-IDPROMR = SPACE OR                 
122204           WDB1-BET-KDVALISO = SPACE)                                     
122304                                                                          
122404           MOVE MFS-NUM-FAELT-FEL TO MOD-TISTADAT-ATTR                    
122504           MOVE NEJ TO INDATA-SW                                          
122604        END-IF                                                            
122704     END-IF                                                               
122804                                                                          
122904     IF INDATA-OK                                                         
123004        IF MID-TISTODAT NOT = ALL '+'                                     
123104           IF MID-TISTODAT > ZERO                                         
123204              MOVE MID-TISTODAT TO DAT-I-TIDATUM                          
123304              MOVE 'AAMMDD'     TO DAT-KDDATFORM                          
123404                                                                          
123504              CALL WDATKONV USING DAT-KDDATFORM                           
123604                                  DAT-I-TIDATUM                           
123704                                  DAT-O-TIDATUM                           
123804                                  DAT-KDSVAR                              
123904                                                                          
124004              IF DAT-KDSVAR-OK                                            
124104                 MOVE DAT-TIAAMMDD TO WS-TISTODAT                         
124204                 IF WS-TISTODAT >= DAGENS-DATUM                           
124304                    IF WS-TISTODAT > WS-TISTADAT                          
124404                       MOVE MFS-NUM-FAELT-RAETT TO                        
124504                            MOD-TISTODAT-ATTR                             
124600                    ELSE                                                  
124701                       MOVE MFS-NUM-FAELT-FEL TO MOD-TISTODAT-ATTR        
124802                       MOVE NEJ TO INDATA-SW                              
124903                    END-IF                                                
125004                 ELSE                                                     
125104                    MOVE MFS-NUM-FAELT-FEL TO MOD-TISTODAT-ATTR           
125204                    MOVE NEJ TO INDATA-SW                                 
125304                 END-IF                                                   
125404              ELSE                                                        
125504                 MOVE MFS-NUM-FAELT-FEL TO MOD-TISTODAT-ATTR              
125604                 MOVE NEJ TO INDATA-SW                                    
125704              END-IF                                                      
125804           ELSE                                                           
125904              IF MID-TISTODAT = ZERO                                      
126004                 MOVE MFS-NUM-FAELT-RAETT TO MOD-TISTODAT-ATTR            
126104              ELSE                                                        
126204                 MOVE MFS-NUM-FAELT-FEL TO MOD-TISTODAT-ATTR              
126304                 MOVE NEJ TO INDATA-SW                                    
126404              END-IF                                                      
126504           END-IF                                                         
126604        END-IF                                                            
126704     END-IF                                                               
126804     .                                                                    
126904                                                                          
127004 CE-KOLLA-DC-DAY-ALT SECTION.                                             
127104                                                                          
127204     IF MID-IDDC-DAY-ALT NOT = ALL '+'                                    
127304       IF MID-TIHHMM-START = ALL '+' OR MID-TIHHMM-STOP = ALL '+'         
127404         MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDDC-DAY-ALT-ATTR                
127504         MOVE MFS-NUM-FAELT-FEL   TO MOD-TIHHMM-START-ATTR                
127604         MOVE MFS-NUM-FAELT-FEL   TO MOD-TIHHMM-STOP-ATTR                 
127704         MOVE NEJ TO INDATA-SW                                            
127804       ELSE                                                               
127904         IF MID-IDDC-DAY-ALT = SPACE AND MID-TIHHMM-START = ZERO          
128004                                     AND MID-TIHHMM-STOP  = ZERO          
128104           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDDC-DAY-ALT-ATTR             
128204         ELSE                                                             
128304           IF DCS-IDDC NOT = MID-IDDC-DAY-ALT                             
128404              MOVE MID-IDDC-DAY-ALT TO W-IDDC-B6                          
128504              PERFORM IMS-GU-WDB601                                       
128604           END-IF                                                         
128704           IF NOT DCS-SDC                                                 
128804             MOVE MFS-ALFA-FAELT-FEL TO MOD-IDDC-DAY-ALT-ATTR             
128904             MOVE NEJ TO INDATA-SW                                        
129004           ELSE                                                           
129104             MOVE MID-IDDC-DAY-ALT   TO W-IDDC-WDB3                       
129204                                        W-IDDC-WDB3-DEF                   
129304             PERFORM IMS-GU-WDB301                                        
129404             IF SEGMENT-FINNS                                             
129504               IF WDB3-DC-KDGENFRA-DO = +0                                
129604                 MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDDC-DAY-ALT-ATTR        
129704                 MOVE NEJ TO INDATA-SW                                    
129804               ELSE                                                       
129904                MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDDC-DAY-ALT-ATTR        
130004               END-IF                                                     
130104             ELSE                                                         
130204               MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDDC-DAY-ALT-ATTR          
130304               MOVE NEJ TO INDATA-SW                                      
130404             END-IF                                                       
130504           END-IF                                                         
130600         END-IF                                                           
130704       END-IF                                                             
130804     END-IF                                                               
130904                                                                          
131004     IF MID-TIHHMM-START NOT = ALL '+'                                    
131104       IF MID-TIHHMM-STOP = ALL '+' OR  MID-IDDC-DAY-ALT = ALL '+'        
131204         MOVE MFS-NUM-FAELT-FEL   TO MOD-TIHHMM-STOP-ATTR                 
131304         MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDDC-DAY-ALT-ATTR                
131404         MOVE NEJ TO INDATA-SW                                            
131504       ELSE                                                               
131604         IF MID-TIHHMM-START < 2400                                       
131704           MOVE MFS-NUM-FAELT-RAETT TO MOD-TIHHMM-START-ATTR              
131804         ELSE                                                             
131904           MOVE MFS-NUM-FAELT-FEL   TO MOD-TIHHMM-START-ATTR              
132004           MOVE NEJ TO INDATA-SW                                          
132104         END-IF                                                           
132204       END-IF                                                             
132304     END-IF                                                               
132404                                                                          
132504     IF MID-TIHHMM-STOP  NOT = ALL '+'                                    
132604       IF MID-TIHHMM-START = ALL '+' OR MID-IDDC-DAY-ALT = ALL '+'        
132704         MOVE MFS-NUM-FAELT-FEL   TO MOD-TIHHMM-START-ATTR                
132804         MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDDC-DAY-ALT-ATTR                
132904         MOVE NEJ TO INDATA-SW                                            
133004       ELSE                                                               
133104         IF (MID-TIHHMM-STOP  < 2400 AND                                  
133204             MID-TIHHMM-STOP  > MID-TIHHMM-START) OR                      
133304            (MID-TIHHMM-STOP = ZERO AND MID-IDDC-DAY-ALT = SPACE)         
133404           MOVE MFS-NUM-FAELT-RAETT TO MOD-TIHHMM-STOP-ATTR               
133504         ELSE                                                             
133604           MOVE MFS-NUM-FAELT-FEL   TO MOD-TIHHMM-STOP-ATTR               
133704           MOVE NEJ TO INDATA-SW                                          
133804         END-IF                                                           
133904       END-IF                                                             
134004     END-IF                                                               
134104     .                                                                    
134204                                                                          
134304 D-UPPDATERA SECTION.                                                     
134404     MOVE 'D-UPPDATERA     ' TO CURR-SECTION.                             
134504                                                                          
134604     PERFORM IMS-GHU-WDB201                                               
134704     IF SEGMENT-FINNS                                                     
134804        PERFORM DA-UPPDATERA-IDDC                                         
134900                                                                          
135004        IF MID-KVDAGAR-DOW NOT = ALL '+'                                  
135104           MOVE MID-KVDAGAR-DOW TO WDB2-GMT-KVDAGAR-DOW                   
135204           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVDAGAR-DOW-ATTR             
135304        ELSE                                                              
135404           MOVE MFS-ROER-EJ-FAELT TO MOD-KVDAGAR-DOW-ATTR                 
135504        END-IF                                                            
135604                                                                          
135704        IF MID-IDDC-DAY-ALT NOT = ALL '+'                                 
135804           MOVE MID-IDDC-DAY-ALT TO WDB2-GMT-IDDC-DAY-ALT                 
135904           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDDC-DAY-ALT-ATTR            
136004        ELSE                                                              
136104           MOVE MFS-ROER-EJ-FAELT TO MOD-IDDC-DAY-ALT-ATTR                
136204        END-IF                                                            
136304                                                                          
136404        IF MID-TIHHMM-START NOT = ALL '+'                                 
136504           MOVE MID-TIHHMM-START TO WDB2-GMT-TIHHMM-START                 
136604           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TIHHMM-START-ATTR            
136704        ELSE                                                              
136804           MOVE MFS-ROER-EJ-FAELT TO MOD-TIHHMM-START-ATTR                
136904        END-IF                                                            
137004                                                                          
137104        IF MID-TIHHMM-STOP  NOT = ALL '+'                                 
137204           MOVE MID-TIHHMM-STOP  TO WDB2-GMT-TIHHMM-STOP                  
137304           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TIHHMM-STOP-ATTR             
137404        ELSE                                                              
137504           MOVE MFS-ROER-EJ-FAELT TO MOD-TIHHMM-STOP-ATTR                 
137604        END-IF                                                            
137704                                                                          
137804        IF MID-TISTADAT NOT = ALL '+'                                     
137904           MOVE WS-TISTADAT  TO WDB2-GMT-TISTADAT                         
138004           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TISTADAT-ATTR                
138104        ELSE                                                              
138204           MOVE MFS-ROER-EJ-FAELT TO MOD-TISTADAT-ATTR                    
138304        END-IF                                                            
138404                                                                          
138504        IF MID-TISTODAT NOT = ALL '+'                                     
138604           MOVE WS-TISTODAT  TO WDB2-GMT-TISTODAT                         
138704           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TISTODAT-ATTR                
138804        ELSE                                                              
138904           MOVE MFS-ROER-EJ-FAELT TO MOD-TISTODAT-ATTR                    
139004        END-IF                                                            
139104                                                                          
139204        PERFORM IMS-REPL-WDB201                                           
139304                                                                          
139404       IF INGEN-UPPDATERING-GJORD                                         
139504          MOVE ALL '+'        TO MID-TABELL-A(1)                          
139604          MOVE ALL '+'        TO MID-TABELL-A(2)                          
139704          MOVE ALL '+'        TO MID-TABELL-A(3)                          
139804          MOVE ALL '+'        TO MID-TABELL-A(4)                          
139904          MOVE ALL '+'        TO MID-TABELL-A(5)                          
140000          MOVE ALL '+'        TO MID-TABELL-A(6)                          
140104          MOVE ALL '+'        TO MID-TABELL-A(7)                          
140207          MOVE ALL '+'        TO MID-TABELL-A(8)                          
140307       END-IF                                                             
140407                                                                          
140507       IF MID-INPUT = ALL '+'                                             
140607          MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                       
140707          CALL WMEDKONV USING MED-WMEDAREA                                
140807          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                 
140907          PERFORM MFS-ROER-EJ-FAELT-IN                                    
141007          PERFORM MFS-ROER-EJ-FAELT-UT                                    
141107       ELSE                                                               
141207          IF UPPDATERING-GJORD                                            
141307             IF BETALARE-OK                                               
141407                MOVE INF-UPDATE-DONE TO MED-IDMFSINF                      
141507                CALL WMEDKONV USING MED-WMEDAREA                          
141607                MOVE MED-MFSINF TO MOD-TEMFSINF                           
141707             ELSE                                                         
141807                MOVE                                                      
141907            'UPDATE DONE, WRONG FIN.CUST. PLEASE UPDATE ON 4412'          
142007                                TO MOD-TEMFSINF                           
142107             END-IF                                                       
142207             PERFORM MFS-FORM-ATTR                                        
142307             IF BETALARE-FEL                                              
142407                MOVE MFS-ALFA-FAELT-FEL TO MOD-TEMFSINF-ATTR              
142507             END-IF                                                       
142607             PERFORM MFS-RENSA-FAELT-IN                                   
142707* * * MFS-ROR-EJ-FAELT TILL FASTA VÄRDEN                                  
142807          END-IF                                                          
142907       END-IF                                                             
143007     END-IF                                                               
143107                                                                          
143207                                                                          
143307     PERFORM IMS-REPL-WDB201                                              
143407                                                                          
143507     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
143607     CALL WMEDKONV USING MED-WMEDAREA                                     
143707     MOVE MED-MFSINF      TO MOD-TEMFSINF                                 
143807                                                                          
143907     PERFORM MFS-FORM-ATTR                                                
144007     PERFORM MFS-RENSA-FAELT-IN                                           
144107     .                                                                    
144207                                                                          
144307                                                                          
144407 DA-UPPDATERA-IDDC SECTION.                                               
144507                                                                          
144607     MOVE +1 TO INDX                                                      
144707     PERFORM UNTIL INDX > MAX-INDX                                        
144807        MOVE WDB2-GMT-IDDC-BULK(INDX) TO W-IDDC-BULK(INDX)                
144907        MOVE SPACE TO WDB2-GMT-IDDC-BULK(INDX)                            
145007                                                                          
145107        MOVE WDB2-GMT-IDDC-DAY (INDX) TO W-IDDC-DAY (INDX)                
145207        MOVE SPACE TO WDB2-GMT-IDDC-DAY (INDX)                            
145307                                                                          
145407        MOVE WDB2-GMT-IDDC-VOR (INDX) TO W-IDDC-VOR (INDX)                
145507        MOVE SPACE TO WDB2-GMT-IDDC-VOR (INDX)                            
145607       ADD +1 TO INDX                                                     
145707     END-PERFORM                                                          
145807                                                                          
145907     MOVE +1  TO INDX                                                     
146007     MOVE +1  TO INDX-BULK                                                
146107     MOVE +1  TO INDX-DAY                                                 
146207     MOVE +1  TO INDX-VOR                                                 
146307     MOVE NEJ TO UPD-SW                                                   
146407     PERFORM UNTIL INDX > MAX-INDX                                        
146507       IF MID-IDDC-BULK (INDX) = ALL '+'                                  
146607         CONTINUE                                                         
146707       ELSE                                                               
146807         MOVE MID-IDDC-BULK(INDX) TO WDB2-GMT-IDDC-BULK(INDX-BULK)        
146907         ADD +1 TO INDX-BULK                                              
147007       END-IF                                                             
147107                                                                          
147207       IF MID-IDDC-DAY (INDX) = ALL '+'                                   
147307         CONTINUE                                                         
147407       ELSE                                                               
147507         MOVE MID-IDDC-DAY (INDX) TO WDB2-GMT-IDDC-DAY (INDX-DAY)         
147607         ADD +1 TO INDX-DAY                                               
147707       END-IF                                                             
147807                                                                          
147907       IF MID-IDDC-VOR (INDX) = ALL '+'                                   
148007         CONTINUE                                                         
148107       ELSE                                                               
148207         MOVE MID-IDDC-VOR (INDX) TO WDB2-GMT-IDDC-VOR (INDX-VOR)         
148307         ADD +1 TO INDX-VOR                                               
148407       END-IF                                                             
148507                                                                          
148607       ADD +1 TO INDX                                                     
148707     END-PERFORM                                                          
148807                                                                          
148907     MOVE +1  TO INDX                                                     
149007     PERFORM UNTIL INDX > MAX-INDX                                        
149107         IF WDB2-GMT-IDDC-BULK(INDX) NOT =                                
149207                   W-IDDC-BULK(INDX)                                      
149307            MOVE JA TO UPD-SW                                             
149407         END-IF                                                           
149507         IF WDB2-GMT-IDDC-DAY (INDX) NOT =                                
149607                   W-IDDC-DAY (INDX)                                      
149707            MOVE JA TO UPD-SW                                             
149807         END-IF                                                           
149907         IF WDB2-GMT-IDDC-VOR (INDX) NOT =                                
150007                   W-IDDC-VOR (INDX)                                      
150107            MOVE JA TO UPD-SW                                             
150207         END-IF                                                           
150307       ADD +1 TO INDX                                                     
150407     END-PERFORM                                                          
150507                                                                          
150607     MOVE JA            TO SPEC-SW                                        
150707     MOVE W-IDDISTR     TO TEST-IDDISTR                                   
150807     IF UPPDATERING-GJORD AND DIS108-SPEC                                 
150907       MOVE NEJ            TO SPEC-SW                                     
151007       MOVE +1 TO INDX                                                    
151107       PERFORM UNTIL INDX > MAX-INDX                                      
151207         IF WDB2-GMT-IDDC-BULK(INDX) = SPACE AND                          
151307           MID-IDDC-BULK (INDX) NOT = SPACE                               
151407           MOVE W-IDDC-BULK(INDX) TO                                      
151507                WDB2-GMT-IDDC-BULK(INDX)                                  
151607         ELSE                                                             
151707          IF MID-IDDC-BULK(INDX) = SPACE OR                               
151807            WDB2-GMT-IDDC-BULK(INDX) > SPACE                              
151907            MOVE JA       TO SPEC-SW                                      
152007          END-IF                                                          
152107         END-IF                                                           
152207         IF WDB2-GMT-IDDC-DAY(INDX) = SPACE AND                           
152307           MID-IDDC-DAY (INDX) NOT = SPACE                                
152407           MOVE W-IDDC-DAY(INDX) TO                                       
152507                WDB2-GMT-IDDC-DAY(INDX)                                   
152607         ELSE                                                             
152707          IF MID-IDDC-DAY(INDX) = SPACE OR                                
152807            WDB2-GMT-IDDC-DAY(INDX) > SPACE                               
152907            MOVE JA       TO SPEC-SW                                      
153007          END-IF                                                          
153107         END-IF                                                           
153207         IF WDB2-GMT-IDDC-VOR(INDX) = SPACE AND                           
153307           MID-IDDC-VOR (INDX) NOT = SPACE                                
153407           MOVE W-IDDC-VOR(INDX) TO                                       
153507                WDB2-GMT-IDDC-VOR(INDX)                                   
153607         ELSE                                                             
153707          IF MID-IDDC-VOR(INDX) = SPACE OR                                
153807            WDB2-GMT-IDDC-VOR(INDX) > SPACE                               
153907            MOVE JA       TO SPEC-SW                                      
154007          END-IF                                                          
154107         END-IF                                                           
154207         ADD +1  TO INDX                                                  
154307       END-PERFORM                                                        
154407     END-IF                                                               
154507                                                                          
154607     IF UPPDATERING-GJORD AND SPEC-JA                                     
154707        MOVE MSGI-IDUSER  TO WDB2-GMT-IDUSER-DCUPD                        
154807        MOVE DAGENS-DATUM TO WDB2-GMT-TIAAMMDD-DCUPD                      
154907     END-IF                                                               
155007                                                                          
155107     IF UPPDATERING-GJORD                                                 
155207        IF W-IDDC-BULK(1) NOT = WDB2-GMT-IDDC-BULK(1)                     
155307           PERFORM DAA-KOLLA-IDFTG                                        
155407           PERFORM DAB-KOLLA-BETALARE                                     
155507        END-IF                                                            
155607     END-IF                                                               
155707     .                                                                    
155807     EJECT                                                                
155907                                                                          
156007 DAA-KOLLA-IDFTG SECTION.                                                 
156107                                                                          
156207     IF DIST35-NONVCC-REFILL                                              
156307     OR DIST07-NA-CUSTOMERS                                               
156407     OR W-IDKUNDNR = ZERO                                                 
156507        CONTINUE                                                          
156607     ELSE                                                                 
156707        MOVE WDB2-GMT-IDDC-BULK(1) TO W-IDDC-B6                           
156807        PERFORM IMS-GU-WDB601                                             
156907        IF SEGMENT-FINNS                                                  
157007           MOVE DCS-IDFTG       TO WDB2-GMT-IDFTG                         
157107        END-IF                                                            
157207     END-IF                                                               
157307     .                                                                    
157407     EJECT                                                                
157507                                                                          
157607 DAB-KOLLA-BETALARE SECTION.                                              
157707                                                                          
157807     MOVE JA TO BETALARE-SW                                               
157907*    PERFORM IMS-GHU-WDB201                                               
158007*    IF SEGMENT-FINNS                                                     
158107       IF WDB2-GMT-IDFTG > ZERO                                           
158207         MOVE WDB2-GMT-IDPARTNR TO W-WDB1-IDPARTNR                        
158307         MOVE WDB2-GMT-IDFTG    TO W-WDB1-IDFTG                           
158407                                                                          
158507         PERFORM IMS-GU-WDB101                                            
158607         IF SEGMENT-SAKNAS                                                
158707            MOVE NEJ TO BETALARE-SW                                       
158807         ELSE                                                             
158907            IF WDB2-GMT-TISTADAT > 0                                      
159007            AND WDB2-GMT-FLOKFAK-R = 'J'                                  
159107            AND (WDB1-BET-IDPARTNR = SPACE                                
159207            OR WDB1-BET-IDFTG   = SPACE)                                  
159307              MOVE NEJ TO BETALARE-SW                                     
159407            END-IF                                                        
159507            IF WDB1-BET-IDPROMR = SPACE                                   
159607            OR WDB1-BET-KDVALISO = SPACE                                  
159707              MOVE NEJ TO BETALARE-SW                                     
159807            END-IF                                                        
159907         END-IF                                                           
160007       END-IF                                                             
160107*    END-IF                                                               
160207     .                                                                    
160307 E-FOERSTA-SIDA SECTION.                                                  
160407                                                                          
160507     PERFORM MFS-RENSA-FAELT-IN                                           
160607     .                                                                    
160707                                                                          
160807                                                                          
160907 F-SAMMA-SIDA SECTION.                                                    
161007                                                                          
161107     PERFORM IMS-GHU-WDB201                                               
161207     IF SEGMENT-FINNS                                                     
161307                                                                          
161407     MOVE +1  TO INDX                                                     
161507     MOVE NEJ TO UPD-SW                                                   
161607     PERFORM UNTIL INDX > MAX-INDX                                        
161707         IF WDB2-GMT-IDDC-BULK(INDX)  = MID-IDDC-BULK(INDX)               
161807         OR (WDB2-GMT-IDDC-BULK(INDX) = SPACE                             
161907             AND MID-IDDC-BULK(INDX)  = SPACE OR LOW-VALUE)               
162007            MOVE MFS-NUM-FIELD-OK  TO MOD-IDDC-BULK-ATTR(INDX)            
162107         ELSE                                                             
162207            MOVE JA TO UPD-SW                                             
162307         END-IF                                                           
162407         IF WDB2-GMT-IDDC-DAY(INDX)   = MID-IDDC-DAY(INDX)                
162507         OR (WDB2-GMT-IDDC-DAY(INDX)  = SPACE                             
162607             AND MID-IDDC-DAY(INDX)   = SPACE OR LOW-VALUE)               
162707            MOVE MFS-NUM-FIELD-OK  TO MOD-IDDC-DAY-ATTR(INDX)             
162807         ELSE                                                             
162907            MOVE JA TO UPD-SW                                             
163007         END-IF                                                           
163107         IF WDB2-GMT-IDDC-VOR(INDX)   = MID-IDDC-VOR(INDX)                
163207         OR (WDB2-GMT-IDDC-VOR(INDX)  = SPACE                             
163307             AND MID-IDDC-VOR(INDX)   = SPACE OR LOW-VALUE)               
163407            MOVE MFS-NUM-FIELD-OK  TO MOD-IDDC-VOR-ATTR(INDX)             
163507         ELSE                                                             
163607            MOVE JA TO UPD-SW                                             
163707         END-IF                                                           
163807         IF WDB2-GMT-IDDC-DAY (INDX) NOT =                                
163907                 MID-IDDC-DAY (INDX)                                      
164007            MOVE JA TO UPD-SW                                             
164107         END-IF                                                           
164207         IF WDB2-GMT-IDDC-VOR (INDX) NOT =                                
164307                 MID-IDDC-VOR (INDX)                                      
164407            MOVE JA TO UPD-SW                                             
164507         END-IF                                                           
164607        ADD +1 TO INDX                                                    
164707     END-PERFORM                                                          
164807     END-IF                                                               
164907                                                                          
165007     IF INGEN-UPPDATERING-GJORD                                           
165107        MOVE ALL '+'          TO MID-TABELL-A(1)                          
165207        MOVE ALL '+'          TO MID-TABELL-A(2)                          
165307        MOVE ALL '+'          TO MID-TABELL-A(3)                          
165407        MOVE ALL '+'          TO MID-TABELL-A(4)                          
165507        MOVE ALL '+'          TO MID-TABELL-A(5)                          
165607        MOVE ALL '+'          TO MID-TABELL-A(6)                          
165707        MOVE ALL '+'          TO MID-TABELL-A(7)                          
165807        MOVE ALL '+'          TO MID-TABELL-A(8)                          
165907     END-IF                                                               
166007                                                                          
166107     IF EGEN-MID OR HELP-MID                                              
166207       IF MID-INPUT = ALL '+'                                             
166307         PERFORM MFS-RENSA-FAELT-IN                                       
166407         MOVE JA TO ALLT-SW                                               
166507       ELSE                                                               
166607         MOVE NEJ TO ALLT-SW                                              
166707         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
166807         CALL WMEDKONV USING MED-WMEDAREA                                 
166907         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
167007         PERFORM MFS-ROER-EJ-FAELT-IN                                     
167107         PERFORM MFS-ROER-EJ-FAELT-UT                                     
167207         PERFORM FA-MID-INDATA-TILL-MOD                                   
167307       END-IF                                                             
167407     ELSE                                                                 
167507       PERFORM MFS-RENSA-FAELT-IN                                         
167607     END-IF                                                               
167707     .                                                                    
167807     EJECT                                                                
167907 FA-MID-INDATA-TILL-MOD SECTION.                                          
168007                                                                          
168107     MOVE +1 TO INDX                                                      
168207     PERFORM UNTIL INDX > MAX-INDX                                        
168307       IF MID-IDDC-BULK (INDX) NOT = ALL '+'                              
168407         MOVE MID-IDDC-BULK (INDX)  TO MOD-IDDC-BULK (INDX)               
168507         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDDC-BULK-ATTR (INDX)          
168607       ELSE                                                               
168707         MOVE MFS-ROER-EJ-FAELT     TO MOD-IDDC-BULK (INDX)               
168807       END-IF                                                             
168907                                                                          
169007       IF MID-IDDC-DAY (INDX) NOT = ALL '+'                               
169107         MOVE MID-IDDC-DAY (INDX) TO MOD-IDDC-DAY (INDX)                  
169207         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDDC-DAY-ATTR (INDX)           
169307       ELSE                                                               
169407         MOVE MFS-ROER-EJ-FAELT     TO MOD-IDDC-DAY (INDX)                
169507       END-IF                                                             
169607                                                                          
169707       IF MID-IDDC-VOR (INDX) NOT = ALL '+'                               
169807         MOVE MID-IDDC-VOR (INDX) TO MOD-IDDC-VOR (INDX)                  
169907         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDDC-VOR-ATTR (INDX)           
170007       ELSE                                                               
170107         MOVE MFS-ROER-EJ-FAELT     TO MOD-IDDC-VOR (INDX)                
170207       END-IF                                                             
170307                                                                          
170407       ADD +1 TO INDX                                                     
170507     END-PERFORM                                                          
170607                                                                          
170707     IF MID-KVDAGAR-DOW NOT = ALL '+'                                     
170807       MOVE MID-KVDAGAR-DOW       TO MOD-KVDAGAR-DOW                      
170907       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVDAGAR-DOW-ATTR                 
171007     ELSE                                                                 
171107       MOVE MFS-ROER-EJ-FAELT     TO MOD-KVDAGAR-DOW                      
171207     END-IF                                                               
171307                                                                          
171407     IF MID-IDDC-DAY-ALT NOT = ALL '+'                                    
171507       MOVE MID-IDDC-DAY-ALT      TO MOD-IDDC-DAY-ALT                     
171607       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDDC-DAY-ALT-ATTR                
171707     ELSE                                                                 
171807       MOVE MFS-ROER-EJ-FAELT     TO MOD-IDDC-DAY-ALT                     
171907     END-IF                                                               
172007                                                                          
172107     IF MID-TIHHMM-START NOT = ALL '+'                                    
172207       MOVE MID-TIHHMM-START      TO MOD-TIHHMM-START                     
172307       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TIHHMM-START-ATTR                
172407     ELSE                                                                 
172507       MOVE MFS-ROER-EJ-FAELT     TO MOD-TIHHMM-START                     
172607     END-IF                                                               
172707                                                                          
172807     IF MID-TIHHMM-STOP  NOT = ALL '+'                                    
172907       MOVE MID-TIHHMM-STOP       TO MOD-TIHHMM-STOP                      
173007       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TIHHMM-STOP-ATTR                 
173107     ELSE                                                                 
173207       MOVE MFS-ROER-EJ-FAELT     TO MOD-TIHHMM-STOP                      
173307     END-IF                                                               
173407                                                                          
173507     IF MID-TISTADAT NOT = ALL '+'                                        
173607       MOVE MID-TISTADAT          TO MOD-TISTADAT                         
173707       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TISTADAT-ATTR                    
173807     ELSE                                                                 
173907       MOVE MFS-ROER-EJ-FAELT     TO MOD-TISTADAT                         
174007     END-IF                                                               
174107                                                                          
174207     IF MID-TISTODAT NOT = ALL '+'                                        
174307       MOVE MID-TISTODAT          TO MOD-TISTODAT                         
174407       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TISTODAT-ATTR                    
174507     ELSE                                                                 
174607       MOVE MFS-ROER-EJ-FAELT     TO MOD-TISTODAT                         
174707     END-IF                                                               
174807                                                                          
174907     .                                                                    
175007 G-LAES-VISA-INFO SECTION.                                                
175107     MOVE 'G-LAES-VISA-INFO' TO CURR-SECTION.                             
175207                                                                          
175307     PERFORM IMS-GU-WDB201                                                
175407                                                                          
175507     IF SEGMENT-SAKNAS                                                    
175607        MOVE 'GOODS RECEIVER MISSING'  TO MOD-TEMFSFEL                    
175707        PERFORM MFS-RENSA-FAELT-UT                                        
175807        PERFORM MFS-RENSA-FAELT-IN                                        
175907     ELSE                                                                 
176007       MOVE WDB2-GMT-KVDAGAR-DOW         TO MOD-KVDAGAR-DOW               
176107       MOVE WDB2-GMT-IDDC-DAY-ALT        TO MOD-IDDC-DAY-ALT              
176207       MOVE WDB2-GMT-TIHHMM-START        TO MOD-TIHHMM-START              
176307       MOVE WDB2-GMT-TIHHMM-STOP         TO MOD-TIHHMM-STOP               
176407       MOVE WDB2-GMT-TISTADAT            TO MOD-TISTADAT                  
176507       MOVE WDB2-GMT-TISTODAT            TO MOD-TISTODAT                  
176607       MOVE WDB2-GMT-IDUSER-DCUPD        TO MOD-IDUSER-DCUPD              
176707       MOVE WDB2-GMT-TIAAMMDD-DCUPD      TO MOD-TIAAMMDD-DCUPD            
176807       MOVE WDB2-GMT-TIFAKT              TO MOD-TIFAKT                    
176907       PERFORM GA-LAES-VISA-DC-FC                                         
177007       PERFORM GB-VISA-LDC                                                
177107     END-IF                                                               
177207     .                                                                    
177307     EJECT                                                                
177407 GA-LAES-VISA-DC-FC SECTION.                                              
177507                                                                          
177607     MOVE +1 TO INDX                                                      
177707     PERFORM UNTIL INDX > MAX-INDX                                        
177807       MOVE WDB2-GMT-IDDC-BULK (INDX)  TO MOD-IDDC-BULK (INDX)            
177907                                          W-IDDC-WDB3                     
178007                                          W-IDDC-WDB3-DEF                 
178107       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-IDDC-BULK-ATTR (INDX)           
178207       PERFORM IMS-GU-WDB301                                              
178307       IF SEGMENT-FINNS                                                   
178407         MOVE WDB3-DC-KDGENFRA-MO  TO MOD-KDGENFRA-MO (INDX)              
178507       ELSE                                                               
178607         MOVE MFS-RENSA-FAELT      TO MOD-KDGENFRA-MO (INDX)              
178707       END-IF                                                             
178807                                                                          
178907       MOVE WDB2-GMT-IDDC-DAY (INDX)  TO MOD-IDDC-DAY (INDX)              
179007                                         W-IDDC-WDB3                      
179107                                         W-IDDC-WDB3-DEF                  
179207       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-IDDC-DAY-ATTR (INDX)            
179307       PERFORM IMS-GU-WDB301                                              
179407       IF SEGMENT-FINNS                                                   
179507         MOVE WDB3-DC-KDGENFRA-DO  TO MOD-KDGENFRA-DO (INDX)              
179607         MOVE WDB3-DC-KVDAGAR-TRP-DAY TO MOD-KVDAGAR-DAY (INDX)           
179707       ELSE                                                               
179807         MOVE MFS-RENSA-FAELT      TO MOD-KDGENFRA-DO (INDX)              
179907       END-IF                                                             
180007                                                                          
180107       MOVE WDB2-GMT-IDDC-VOR (INDX)  TO MOD-IDDC-VOR (INDX)              
180207                                         W-IDDC-WDB3                      
180307                                         W-IDDC-WDB3-DEF                  
180407       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-IDDC-VOR-ATTR (INDX)            
180507       PERFORM IMS-GU-WDB301                                              
180607       IF SEGMENT-FINNS                                                   
180707         MOVE WDB3-DC-KDGENFRA-VOR TO MOD-KDGENFRA-VOR (INDX)             
180807       ELSE                                                               
180907         MOVE MFS-RENSA-FAELT      TO MOD-KDGENFRA-VOR (INDX)             
181007       END-IF                                                             
181107                                                                          
181207       ADD +1 TO INDX                                                     
181307     END-PERFORM                                                          
181407     .                                                                    
181507                                                                          
181607 GB-VISA-LDC        SECTION.                                              
181707                                                                          
181807     CONTINUE                                                             
181907     .                                                                    
182007                                                                          
182107 S01-KOLLA-KINA-DC  SECTION.                                              
182207                                                                          
182307     MOVE +1 TO INDX                                                      
182407     MOVE +0 TO ANTAL-CDC                                                 
182507     MOVE +0 TO ANTAL-LDC                                                 
182607     MOVE +0 TO ANTAL-NDC                                                 
182707     MOVE +0 TO ANTAL-NDC-SH                                              
182807                                                                          
182907*    KOLLA ORDNING                                                        
183007     PERFORM UNTIL INDX > MAX-INDX                                        
183107                                                                          
183207        IF BULK-KL                                                        
183307           MOVE MID-IDDC-BULK (INDX) TO WS-IDDC                           
183407                                                                          
183507           IF WS-IDDC NOT = SPACE                                         
183607              IF LDC-CN AND ANTAL-NDC = ZERO                              
183707                 ADD +1          TO ANTAL-LDC                             
183807              ELSE                                                        
183907                 IF NDC-CN                                                
184007                    ADD +1       TO ANTAL-NDC                             
184107                    IF NDC-CN-71                                          
184207                       ADD +1    TO ANTAL-NDC-SH                          
184307                    END-IF                                                
184407                 ELSE                                                     
184507                    IF CDC-SE                                             
184607                       ADD +1    TO ANTAL-CDC                             
184707                    ELSE                                                  
184807                       MOVE MFS-ALFA-FAELT-FEL                            
184907                                TO MOD-IDDC-BULK-ATTR (INDX)              
185009                       MOVE NEJ      TO INDATA-SW                         
185109                       MOVE MAX-INDX TO INDX                              
185207                    END-IF                                                
185307                 END-IF                                                   
185407              END-IF                                                      
185507           END-IF                                                         
185607        END-IF                                                            
185707                                                                          
185807        IF DAY-KL                                                         
185907           MOVE MID-IDDC-DAY (INDX) TO WS-IDDC                            
186007                                                                          
186107           IF WS-IDDC NOT = SPACE                                         
186207              IF LDC-CN AND ANTAL-NDC = ZERO                              
186307                 ADD +1          TO ANTAL-LDC                             
186407              ELSE                                                        
186507                 IF NDC-CN                                                
186607                    ADD +1       TO ANTAL-NDC                             
186707                    IF NDC-CN-71                                          
186807                       ADD +1    TO ANTAL-NDC-SH                          
186907                    END-IF                                                
187007                 ELSE                                                     
187107                    IF CDC-SE                                             
187207                       ADD +1    TO ANTAL-CDC                             
187307                    ELSE                                                  
187407                       MOVE MFS-ALFA-FAELT-FEL                            
187507                                TO MOD-IDDC-DAY-ATTR (INDX)               
187609                       MOVE NEJ      TO INDATA-SW                         
187709                       MOVE MAX-INDX TO INDX                              
187807                    END-IF                                                
187907                 END-IF                                                   
188007              END-IF                                                      
188107           END-IF                                                         
188207        END-IF                                                            
188307                                                                          
188407        IF VOR-KL                                                         
188507           MOVE MID-IDDC-VOR (INDX) TO WS-IDDC                            
188607                                                                          
188707           IF WS-IDDC NOT = SPACE                                         
188807              IF LDC-CN AND ANTAL-NDC = ZERO                              
188907                 ADD +1          TO ANTAL-LDC                             
189007              ELSE                                                        
189107                 IF NDC-CN                                                
189207                    ADD +1       TO ANTAL-NDC                             
189307                    IF NDC-CN-71                                          
189407                       ADD +1    TO ANTAL-NDC-SH                          
189507                    END-IF                                                
189607                 ELSE                                                     
189707                    IF CDC-SE                                             
189807                       ADD +1    TO ANTAL-CDC                             
189907                    ELSE                                                  
190007                       MOVE MFS-ALFA-FAELT-FEL                            
190107                                TO MOD-IDDC-VOR-ATTR (INDX)               
190209                       MOVE NEJ      TO INDATA-SW                         
190309                       MOVE MAX-INDX TO INDX                              
190407                    END-IF                                                
190507                 END-IF                                                   
190607              END-IF                                                      
190707           END-IF                                                         
190807        END-IF                                                            
190907        ADD +1 TO INDX                                                    
191007     END-PERFORM                                                          
191107                                                                          
191207     IF ANTAL-NDC-SH = ZERO                                               
191307        PERFORM S04-FELMARKERA-SISTA-DC                                   
191407        MOVE NEJ                 TO INDATA-SW                             
191507     END-IF                                                               
191607                                                                          
191707     IF ANTAL-CDC > ZERO                                                  
191807        PERFORM S05-KOLLA-CDC                                             
191907     END-IF                                                               
192007     .                                                                    
192107                                                                          
192207 S02-KOLLA-DUBLETTER  SECTION.                                            
192307                                                                          
192407     MOVE +1 TO INDX                                                      
192507                                                                          
192607     PERFORM UNTIL INDX  > MAX-INDX                                       
192707                                                                          
192807        IF BULK-KL                                                        
192907           IF MID-IDDC-BULK (INDX) NOT = SPACE                            
193007              COMPUTE INDX2 = INDX + 1                                    
193107              PERFORM UNTIL INDX2 > MAX-INDX                              
193207                 IF MID-IDDC-BULK (INDX) = MID-IDDC-BULK (INDX2)          
193307                    MOVE NEJ        TO INDATA-SW                          
193407                    MOVE MFS-ALFA-FAELT-FEL                               
193507                                TO MOD-IDDC-BULK-ATTR (INDX)              
193608                    MOVE MAX-INDX   TO INDX                               
193708                    MOVE MAX-INDX   TO INDX2                              
193807                 END-IF                                                   
193907                 ADD +1 TO INDX2                                          
194007              END-PERFORM                                                 
194107           END-IF                                                         
194207        END-IF                                                            
194307                                                                          
194407        IF DAY-KL                                                         
194507           IF MID-IDDC-DAY (INDX) NOT = SPACE                             
194607              COMPUTE INDX2 = INDX + 1                                    
194707              PERFORM UNTIL INDX2 > MAX-INDX                              
194807                 IF MID-IDDC-DAY (INDX) = MID-IDDC-DAY (INDX2)            
194907                    MOVE NEJ        TO INDATA-SW                          
195007                    MOVE MFS-ALFA-FAELT-FEL                               
195107                                TO MOD-IDDC-DAY-ATTR (INDX)               
195208                    MOVE MAX-INDX   TO INDX                               
195308                    MOVE MAX-INDX   TO INDX2                              
195407                 END-IF                                                   
195507                 ADD +1 TO INDX2                                          
195607              END-PERFORM                                                 
195707           END-IF                                                         
195807        END-IF                                                            
195907                                                                          
196007        IF VOR-KL                                                         
196107           IF MID-IDDC-VOR (INDX) NOT = SPACE                             
196207              COMPUTE INDX2 = INDX + 1                                    
196307              PERFORM UNTIL INDX2 > MAX-INDX                              
196407                 IF MID-IDDC-VOR (INDX) = MID-IDDC-VOR (INDX2)            
196507                    MOVE NEJ        TO INDATA-SW                          
196607                    MOVE MFS-ALFA-FAELT-FEL                               
196707                                TO MOD-IDDC-VOR-ATTR (INDX)               
196808                    MOVE MAX-INDX   TO INDX                               
196908                    MOVE MAX-INDX   TO INDX2                              
197007                 END-IF                                                   
197107                 ADD +1 TO INDX2                                          
197207              END-PERFORM                                                 
197307           END-IF                                                         
197407        END-IF                                                            
197507        ADD +1 TO INDX                                                    
197607     END-PERFORM                                                          
197707     .                                                                    
197807                                                                          
197907                                                                          
198007 S03-KOLLA-BLANDNING  SECTION.                                            
198107                                                                          
198207*  OM NÅGOT DC ÄR KINA MÅSTA SAMTLIGA DC VARA KINESISKA                   
198307*  MED UNDANTAG FÖR DC 11 SOM ALLTID ÄR TILLÅTET                          
198407                                                                          
198507     MOVE +1     TO INDX                                                  
198607     MOVE SPACE  TO KINA-SW                                               
198707                                                                          
198807     PERFORM UNTIL INDX  > MAX-INDX                                       
198907        IF MID-IDDC-BULK (INDX) NOT = SPACE                               
199007           IF LDC-CN OR NDC-CN                                            
199107              SET KINA-DC TO TRUE                                         
199207           ELSE                                                           
199307              IF KINA-DC AND NOT CDC-SE                                   
199407                 MOVE NEJ           TO INDATA-SW                          
199507                 IF BULK-KL                                               
199607                    MOVE MFS-ALFA-FAELT-FEL                               
199707                                    TO MOD-IDDC-BULK-ATTR(INDX2)          
199807                 END-IF                                                   
199907                 IF DAY-KL                                                
200007                    MOVE MFS-ALFA-FAELT-FEL                               
200107                                    TO MOD-IDDC-DAY-ATTR(INDX2)           
200207                 END-IF                                                   
200307                 IF VOR-KL                                                
200407                    MOVE MFS-ALFA-FAELT-FEL                               
200507                                    TO MOD-IDDC-VOR-ATTR(INDX2)           
200607                 END-IF                                                   
200707                                                                          
200808                 MOVE MAX-INDX      TO INDX                               
200907              END-IF                                                      
201007           END-IF                                                         
201107        END-IF                                                            
201207        ADD +1 TO INDX                                                    
201307     END-PERFORM                                                          
201407     .                                                                    
201507                                                                          
201607                                                                          
201707 S04-FELMARKERA-SISTA-DC  SECTION.                                        
201807                                                                          
201907*    LETA BAKIFRÅN I RESPEKTIVE DC-KOLUMN MED HJÄLP AV                    
202007*    88-NIVÅER BULK, DAY ELLER VOR SOM SÄTTS FÖRE ANROP AV S02-           
202107                                                                          
202209     MOVE MAX-INDX TO INDX2                                               
202307     PERFORM UNTIL INDX2 < +1                                             
202407        IF BULK-KL                                                        
202507           IF MID-IDDC-BULK(INDX2) = SPACE                                
202607              CONTINUE                                                    
202707           ELSE                                                           
202807              MOVE MFS-ALFA-FAELT-FEL                                     
202907                             TO MOD-IDDC-BULK-ATTR(INDX2)                 
203007              MOVE +0 TO INDX2                                            
203107           END-IF                                                         
203207        END-IF                                                            
203307                                                                          
203407        IF DAY-KL                                                         
203507           IF MID-IDDC-DAY (INDX2) = SPACE                                
203607              CONTINUE                                                    
203707           ELSE                                                           
203807              MOVE MFS-ALFA-FAELT-FEL                                     
203907                             TO MOD-IDDC-DAY-ATTR (INDX2)                 
204007              MOVE +0 TO INDX2                                            
204107           END-IF                                                         
204207        END-IF                                                            
204307                                                                          
204407        IF VOR-KL                                                         
204507           IF MID-IDDC-VOR (INDX2) = SPACE                                
204607              CONTINUE                                                    
204707           ELSE                                                           
204807              MOVE MFS-ALFA-FAELT-FEL                                     
204907                             TO MOD-IDDC-VOR-ATTR (INDX2)                 
205007              MOVE +0 TO INDX2                                            
205107           END-IF                                                         
205207        END-IF                                                            
205307                                                                          
205407        SUBTRACT 1 FROM INDX2                                             
205507     END-PERFORM                                                          
205607     .                                                                    
205707                                                                          
205807                                                                          
205907 S05-KOLLA-CDC            SECTION.                                        
206007                                                                          
206107*    DC11 FÅR FÖREKOMMA EN GÅNG, MEN SKALL DÅ LIGGA SIST                  
206207                                                                          
206309     MOVE MAX-INDX TO INDX2                                               
206407     PERFORM UNTIL INDX2 < +1                                             
206507        IF BULK-KL                                                        
206607           IF MID-IDDC-BULK(INDX2) = SPACE                                
206707              CONTINUE                                                    
206807           ELSE                                                           
206907              MOVE MID-IDDC-BULK(INDX2)                                   
207007                             TO WS-IDDC                                   
207107              IF NOT CDC-SE                                               
207207                 MOVE NEJ    TO INDATA-SW                                 
207307              END-IF                                                      
207407              MOVE +0        TO INDX2                                     
207507           END-IF                                                         
207607        END-IF                                                            
207707                                                                          
207807        IF DAY-KL                                                         
207907           IF MID-IDDC-DAY (INDX2) = SPACE                                
208007              CONTINUE                                                    
208107           ELSE                                                           
208207              MOVE MID-IDDC-DAY (INDX2)                                   
208307                             TO WS-IDDC                                   
208407              IF NOT CDC-SE                                               
208507                 MOVE NEJ    TO INDATA-SW                                 
208607              END-IF                                                      
208707              MOVE +0        TO INDX2                                     
208807           END-IF                                                         
208907        END-IF                                                            
209007                                                                          
209107        IF VOR-KL                                                         
209207           IF MID-IDDC-VOR (INDX2) = SPACE                                
209307              CONTINUE                                                    
209407           ELSE                                                           
209507              MOVE MID-IDDC-VOR (INDX2)                                   
209607                             TO WS-IDDC                                   
209707              IF NOT CDC-SE                                               
209807                 MOVE NEJ    TO INDATA-SW                                 
209907              END-IF                                                      
210007              MOVE +0        TO INDX2                                     
210107           END-IF                                                         
210207        END-IF                                                            
210307                                                                          
210407        SUBTRACT 1 FROM INDX2                                             
210507     END-PERFORM                                                          
210607                                                                          
210707     IF INDATA-FEL                                                        
210807*    DC11 FINNS PÅ FEL PLATS, LETA UPP DET OCH FELMARKERA                 
210907                                                                          
211007        MOVE +1 TO INDX2                                                  
211107        PERFORM UNTIL INDX2 > MAX-INDX                                    
211207           IF BULK-KL                                                     
211307              MOVE MID-IDDC-BULK(INDX2)                                   
211407                               TO WS-IDDC                                 
211507              IF CDC-SE                                                   
211607                 MOVE MFS-ALFA-FAELT-FEL                                  
211707                               TO MOD-IDDC-BULK-ATTR(INDX2)               
211807                 MOVE MAX-INDX TO INDX2                                   
211907              END-IF                                                      
212007           END-IF                                                         
212107                                                                          
212207           IF DAY-KL                                                      
212307              MOVE MID-IDDC-DAY (INDX2)                                   
212407                               TO WS-IDDC                                 
212507              IF CDC-SE                                                   
212607                 MOVE MFS-ALFA-FAELT-FEL                                  
212707                               TO MOD-IDDC-DAY-ATTR(INDX2)                
212807                 MOVE MAX-INDX TO INDX2                                   
212907              END-IF                                                      
213007           END-IF                                                         
213107                                                                          
213207           IF VOR-KL                                                      
213307              MOVE MID-IDDC-VOR (INDX2)                                   
213407                               TO WS-IDDC                                 
213507              IF CDC-SE                                                   
213607                 MOVE MFS-ALFA-FAELT-FEL                                  
213707                               TO MOD-IDDC-VOR-ATTR(INDX2)                
213807                 MOVE MAX-INDX TO INDX2                                   
213907              END-IF                                                      
214007           END-IF                                                         
214107           ADD +1 TO INDX2                                                
214207                                                                          
214307        END-PERFORM                                                       
214407     END-IF                                                               
214507     .                                                                    
214607                                                                          
214707                                                                          
214807 S06-CHECK-WDA5 SECTION.                                                  
214907                                                                          
215007     MOVE SPACES TO WS-DEL-DC                                             
215107                                                                          
215207     PERFORM IMS-GN-WDA501                                                
215307     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
215407             OR WS-DEL-DC = NEJ                                           
215507        IF SEGMENT-FINNS                                                  
215607          IF (RAD-KDSTARAD = '2' OR '3')                                  
215707              MOVE NEJ TO WS-DEL-DC                                       
215807              MOVE ERR-BO-EXISTS   TO MED-IDMFSINF                        
215907          ELSE                                                            
216007              MOVE JA TO WS-DEL-DC                                        
216107          END-IF                                                          
216207        END-IF                                                            
216307        PERFORM IMS-GN-WDA501                                             
216407     END-PERFORM                                                          
216507     .                                                                    
216607                                                                          
216707 S07-KOLLA-MAX-ANTAL  SECTION.                                            
216807                                                                          
216910*  THERE IS A LIMIT OF 7 NDC AND 3 SDC IN THE CLEARGROUP                  
217007*                                                                         
217107                                                                          
217207     MOVE +1     TO INDX                                                  
217307     MOVE ZERO   TO ANTAL-LDC                                             
217407                    ANTAL-SDC                                             
217507                    ANTAL-NDC                                             
217607                                                                          
217707     PERFORM UNTIL INDX  > MAX-INDX                                       
217807        IF BULK-KL                                                        
217907           MOVE MID-IDDC-BULK(INDX) TO WS-IDDC                            
218007        END-IF                                                            
218107        IF DAY-KL                                                         
218207           MOVE MID-IDDC-DAY(INDX) TO WS-IDDC                             
218307        END-IF                                                            
218407        IF VOR-KL                                                         
218507           MOVE MID-IDDC-VOR(INDX) TO WS-IDDC                             
218607        END-IF                                                            
218707                                                                          
218807        IF WS-IDDC NOT = SPACE                                            
218907           IF LDC                                                         
219007              ADD +1 TO ANTAL-LDC                                         
219107           END-IF                                                         
219207           IF SDC                                                         
219307              ADD +1 TO ANTAL-SDC                                         
219407           END-IF                                                         
219507           IF NDC                                                         
219607              ADD +1 TO ANTAL-NDC                                         
219707           END-IF                                                         
219807        END-IF                                                            
219907        ADD +1 TO INDX                                                    
220007     END-PERFORM                                                          
220107                                                                          
220210     IF ANTAL-LDC > 3 OR ANTAL-NDC > 7                                    
220307        MOVE +1  TO INDX                                                  
220407                                                                          
220507        PERFORM UNTIL INDX  > MAX-INDX                                    
220607           IF BULK-KL                                                     
220707              MOVE MID-IDDC-BULK(INDX) TO WS-IDDC                         
220807              IF LDC AND ANTAL-LDC > 3                                    
220910              OR NDC AND ANTAL-NDC > 7                                    
221007                    MOVE MFS-ALFA-FAELT-FEL                               
221107                                    TO MOD-IDDC-BULK-ATTR(INDX)           
221207                 MOVE NEJ    TO INDATA-SW                                 
221307              END-IF                                                      
221407           END-IF                                                         
221507           IF DAY-KL                                                      
221607              MOVE MID-IDDC-DAY(INDX) TO WS-IDDC                          
221707              IF LDC AND ANTAL-LDC > 3                                    
221810              OR NDC AND ANTAL-NDC > 7                                    
221907                    MOVE MFS-ALFA-FAELT-FEL                               
222007                                    TO MOD-IDDC-DAY-ATTR(INDX)            
222107                 MOVE NEJ    TO INDATA-SW                                 
222207              END-IF                                                      
222307           END-IF                                                         
222407           IF VOR-KL                                                      
222507              MOVE MID-IDDC-VOR(INDX) TO WS-IDDC                          
222607              IF LDC AND ANTAL-LDC > 3                                    
222710              OR NDC AND ANTAL-NDC > 7                                    
222807                    MOVE MFS-ALFA-FAELT-FEL                               
222907                                    TO MOD-IDDC-VOR-ATTR(INDX)            
223007                 MOVE NEJ    TO INDATA-SW                                 
223107              END-IF                                                      
223207           END-IF                                                         
223307           ADD +1 TO INDX                                                 
223407        END-PERFORM                                                       
223507     END-IF                                                               
223607     .                                                                    
223707                                                                          
223807                                                                          
223907 S08-CHECK-WDQ2  SECTION.                                                 
224007     PERFORM IMS-GN-WDQ201-CSEQ                                           
224107                                                                          
224207     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
224307             OR WS-DEL-DC = NEJ                                           
224407                                                                          
224507       IF OHUV-KDORDKL >= W-KDORDKL-MIN                                   
224607      AND OHUV-KDORDKL <= W-KDORDKL-MAX                                   
224707                                                                          
224807          MOVE WS-IDDC-WORK TO W-IDDC-Q2                                  
225311          PERFORM IMS-GNP-WDQ221                                          
226011          IF SEGMENT-FINNS                                                
227011             MOVE NEJ TO WS-DEL-DC                                        
228011             MOVE ERR-ORDER-EXISTS TO MED-IDMFSINF                        
229011          END-IF                                                          
230104                                                                          
230204       END-IF                                                             
230304       PERFORM IMS-GN-WDQ201-CSEQ                                         
230404     END-PERFORM                                                          
230504     .                                                                    
230604                                                                          
230704                                                                          
230804 MFS-RENSA-FAELT-UT SECTION.                                              
230904                                                                          
231000*    --- ALLA UTDATA-FÄLT                                                 
231100     MOVE MFS-RENSA-FAELT TO MOD-IDUSER-DCUPD                             
231200                             MOD-TIAAMMDD-DCUPD                           
231304                             MOD-TIFAKT                                   
231400                             MOD-KVDAGAR-DOW                              
231501                             MOD-IDDC-DAY-ALT                             
231602                             MOD-TIHHMM-START                             
231703                             MOD-TIHHMM-STOP                              
231804                             MOD-TISTADAT                                 
231900                             MOD-TISTODAT                                 
232000     MOVE +1 TO INDX                                                      
232100     PERFORM UNTIL INDX > MAX-INDX                                        
232200       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
232300       ADD +1 TO INDX                                                     
232400     END-PERFORM                                                          
232500     .                                                                    
232600                                                                          
232704 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
232804                                                                          
232904     MOVE MFS-RENSA-FAELT TO MOD-IDDC-BULK    (INDX)                      
233004                             MOD-KDGENFRA-MO  (INDX)                      
233104                             MOD-IDDC-DAY     (INDX)                      
233204                             MOD-KDGENFRA-DO  (INDX)                      
233304                             MOD-KVDAGAR-DAY  (INDX)                      
233404                             MOD-IDDC-VOR     (INDX)                      
233504                             MOD-KDGENFRA-VOR (INDX)                      
233604     .                                                                    
233704                                                                          
233804 MFS-RENSA-FAELT-IN SECTION.                                              
233900                                                                          
234000*    --- ALLA INDATA-FÄLT                                                 
234104     MOVE MFS-RENSA-FAELT TO MOD-KVDAGAR-DOW                              
234204                             MOD-IDDC-DAY-ALT                             
234304                             MOD-TIHHMM-START                             
234404                             MOD-TIHHMM-STOP                              
234504                             MOD-TISTADAT                                 
234604                             MOD-TISTODAT                                 
234704                                                                          
234804     MOVE +1 TO INDX                                                      
234904     PERFORM UNTIL INDX > MAX-INDX                                        
235004       PERFORM MFS-RENSA-RAD-FAELT-IN                                     
235104       ADD +1 TO INDX                                                     
235204     END-PERFORM                                                          
235300     .                                                                    
235400     CONTINUE                                                             
235500     .                                                                    
235600                                                                          
235704 MFS-RENSA-RAD-FAELT-IN SECTION.                                          
235804                                                                          
235904     MOVE MFS-RENSA-FAELT TO MOD-IDDC-BULK    (INDX)                      
236004                             MOD-KDGENFRA-MO  (INDX)                      
236104                             MOD-IDDC-DAY     (INDX)                      
236204                             MOD-KDGENFRA-DO  (INDX)                      
236304                             MOD-KVDAGAR-DAY  (INDX)                      
236404                             MOD-IDDC-VOR     (INDX)                      
236504                             MOD-KDGENFRA-VOR (INDX)                      
236604     .                                                                    
236704                                                                          
236804                                                                          
236900 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
237000                                                                          
237100*    --- ALLA UTDATA-FÄLT                                                 
237200     MOVE MFS-ROER-EJ-FAELT TO MOD-KVDAGAR-DOW                            
237300                               MOD-IDDC-DAY-ALT                           
237404                               MOD-TIHHMM-START                           
237500                               MOD-TIHHMM-STOP                            
237600                               MOD-TISTADAT                               
237700                               MOD-TISTODAT                               
237800                               MOD-IDUSER-DCUPD                           
237900                               MOD-TIAAMMDD-DCUPD                         
238000                               MOD-TIFAKT                                 
238104                                                                          
238200     MOVE +1 TO INDX                                                      
238301     PERFORM UNTIL INDX > MAX-INDX                                        
238402       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
238503       ADD +1 TO INDX                                                     
238604     END-PERFORM                                                          
238704     .                                                                    
238804     SKIP3                                                                
238904 MFS-ROER-EJ-RAD-FAELT-UT SECTION.                                        
239004                                                                          
239104     MOVE MFS-ROER-EJ-FAELT TO MOD-IDDC-BULK    (INDX)                    
239204                               MOD-KDGENFRA-MO (INDX)                     
239304                               MOD-IDDC-DAY   (INDX)                      
239404                               MOD-KDGENFRA-DO (INDX)                     
239504                               MOD-KVDAGAR-DAY (INDX)                     
239604                               MOD-IDDC-VOR   (INDX)                      
239704                               MOD-KDGENFRA-VOR (INDX)                    
239804     .                                                                    
239904                                                                          
240000 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
240100                                                                          
240200*    --- ALLA INDATA-FÄLT                                                 
240304     MOVE MFS-ROER-EJ-FAELT TO MOD-KVDAGAR-DOW                            
240404                               MOD-IDDC-DAY-ALT                           
240504                               MOD-TIHHMM-START                           
240604                               MOD-TIHHMM-STOP                            
240704                               MOD-TISTADAT                               
240804                               MOD-TISTODAT                               
240904                                                                          
241004     MOVE +1 TO INDX                                                      
241104     PERFORM UNTIL INDX > MAX-INDX                                        
241204       PERFORM MFS-ROER-EJ-RAD-FAELT-IN                                   
241304       ADD +1 TO INDX                                                     
241404     END-PERFORM                                                          
241500     CONTINUE                                                             
241600     .                                                                    
241700                                                                          
241804 MFS-ROER-EJ-RAD-FAELT-IN SECTION.                                        
241904                                                                          
242004     MOVE MFS-ROER-EJ-FAELT TO MOD-IDDC-BULK    (INDX)                    
242104                               MOD-KDGENFRA-MO  (INDX)                    
242204                               MOD-IDDC-DAY     (INDX)                    
242304                               MOD-KDGENFRA-DO  (INDX)                    
242404                               MOD-KVDAGAR-DAY  (INDX)                    
242504                               MOD-IDDC-VOR     (INDX)                    
242604                               MOD-KDGENFRA-VOR (INDX)                    
242704     .                                                                    
242804                                                                          
242904                                                                          
243000 MFS-FORM-ATTR SECTION.                                                   
243100                                                                          
243200*    --- ALLA INDATA-FÄLT                                                 
243300     MOVE MFS-FORMATETS-ATTR TO MOD-KVDAGAR-DOW-ATTR                      
243400                                MOD-IDDC-DAY-ALT-ATTR                     
243504                                MOD-TIHHMM-START-ATTR                     
243600                                MOD-TIHHMM-STOP-ATTR                      
243700                                MOD-TISTADAT-ATTR                         
243800                                MOD-TISTODAT-ATTR                         
243900                                MOD-TEMFSINF-ATTR                         
244000     MOVE +1 TO INDX                                                      
244100     PERFORM UNTIL INDX > MAX-INDX                                        
244200       PERFORM MFS-RAD-FORM-ATTR                                          
244300       ADD +1 TO INDX                                                     
244400     END-PERFORM                                                          
244500     .                                                                    
244600                                                                          
244704 MFS-RAD-FORM-ATTR SECTION.                                               
244804                                                                          
244904     MOVE MFS-FORMATETS-ATTR TO MOD-IDDC-BULK-ATTR (INDX)                 
245004                                MOD-IDDC-DAY-ATTR (INDX)                  
245104                                MOD-IDDC-VOR-ATTR (INDX)                  
245204     .                                                                    
245304                                                                          
245404                                                                          
245500* --- IMS SEKTIONER ---                                                   
245600     SKIP3                                                                
245700 IMS-GET-MSG SECTION.                                                     
245800                                                                          
245900     MOVE '  QC'          TO GODK-STATUSKODER                             
246000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
246100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
246200     PERFORM IMS-STATUSKONTROLL                                           
246300     .                                                                    
246400                                                                          
246500 IMS-INSERT-MSG SECTION.                                                  
246600                                                                          
246700     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
246800       MOVE 'N'           TO MFS-KDHUVOMR                                 
246900     END-IF                                                               
247000     MOVE LOW-VALUE       TO MSG-KDZ1 MSG-KDZ2                            
247100     MOVE SPACE           TO GODK-STATUSKODER                             
247200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
247300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
247400     PERFORM IMS-STATUSKONTROLL                                           
247500     .                                                                    
247604 IMS-GN-WDA501 SECTION.                                                   
247704     MOVE 'IMS-GU-WDA501'   TO CURR-IMS-SECTION                           
247804                                                                          
247904     STRING 'WDA501  (WDA501KY>=' W-WDA5-MIN-X                            
248004                    '&WDA501KY<=' W-WDA5-MAX-X                            
248104                    '&IDDC     =' W-WDA5-IDDC ')'                         
248204            DELIMITED BY SIZE INTO SSA1                                   
248304     MOVE '  GEGB'            TO GODK-STATUSKODER                         
248404     CALL CBLTDLI USING GN WDA5-PCB DLI-IO-WDA501  SSA1                   
248504     MOVE WDA5-STATUS-CODE TO STATUS-WS                                   
248604     PERFORM IMS-STATUSKONTROLL                                           
248704     .                                                                    
248800                                                                          
248904 IMS-GU-WDB101 SECTION.                                                   
249004     MOVE 'IMS-GU-WDB101'     TO CURR-IMS-SECTION                         
249104                                                                          
249204     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
249304          DELIMITED BY SIZE INTO SSA1                                     
249404     MOVE '  GE'              TO GODK-STATUSKODER                         
249504     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101 SSA1                    
249604     MOVE WDB1-STATUS-CODE    TO STATUS-WS                                
249704     PERFORM IMS-STATUSKONTROLL                                           
249804     .                                                                    
249900 IMS-GU-WDB201 SECTION.                                                   
250000     MOVE 'IMS-GU-WDB201'   TO CURR-IMS-SECTION                           
250100                                                                          
250200     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
250300          DELIMITED BY SIZE INTO SSA1                                     
250400     MOVE '  GE'              TO GODK-STATUSKODER                         
250500     CALL CBLTDLI USING GU  WDB2-PCB DLI-IO-AREA SSA1                     
250600     MOVE WDB2-STATUS-CODE    TO STATUS-WS                                
250700     PERFORM IMS-STATUSKONTROLL                                           
250800     .                                                                    
250900                                                                          
251000 IMS-GHU-WDB201 SECTION.                                                  
251100     MOVE 'IMS-GHU-WDB201'   TO CURR-IMS-SECTION                          
251200                                                                          
251300     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
251400          DELIMITED BY SIZE INTO SSA1                                     
251500     MOVE '  GE'              TO GODK-STATUSKODER                         
251600     CALL CBLTDLI USING GHU WDB2-PCB DLI-IO-AREA SSA1                     
251700     MOVE WDB2-STATUS-CODE    TO STATUS-WS                                
251800     PERFORM IMS-STATUSKONTROLL                                           
251900     .                                                                    
252000                                                                          
252100 IMS-REPL-WDB201 SECTION.                                                 
252200     MOVE 'IMS-REPL-WDB201'   TO CURR-IMS-SECTION                         
252300                                                                          
252400     MOVE '  '             TO GODK-STATUSKODER                            
252500     CALL CBLTDLI USING REPL WDB2-PCB DLI-IO-AREA                         
252600     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
252700     PERFORM IMS-STATUSKONTROLL                                           
252800     .                                                                    
252900                                                                          
253004 IMS-GU-WDB301 SECTION.                                                   
253104     MOVE 'IMS-GU-WDB301'   TO CURR-IMS-SECTION                           
253204                                                                          
253304     STRING 'WDB301  (WDB301KY =' W-WDB301KY-X                            
253404                    '+WDB301KY =' W-WDB301KY-DEF-X ')'                    
253504          DELIMITED BY SIZE INTO SSA1                                     
253604     MOVE '  GE' TO GODK-STATUSKODER                                      
253704     CALL CBLTDLI USING GHU WDB3-PCB DLI-IO-WDB301 SSA1                   
253804     MOVE WDB3-STATUS-CODE TO STATUS-WS                                   
253904     PERFORM IMS-STATUSKONTROLL                                           
254004     .                                                                    
254104                                                                          
254200 IMS-GU-WDB601       SECTION.                                             
254300     MOVE 'IMS-GU-WDB601'   TO CURR-IMS-SECTION                           
254400                                                                          
254500     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
254600          DELIMITED BY SIZE INTO SSA1                                     
254700     MOVE '  GE'              TO GODK-STATUSKODER                         
254800     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
254900     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
255000     PERFORM IMS-STATUSKONTROLL                                           
255100     .                                                                    
255200                                                                          
255304 IMS-GN-WDQ201-CSEQ SECTION.                                              
255404     MOVE 'IMS-GN-WDQ201'   TO CURR-IMS-SECTION                           
255504                                                                          
255604     STRING 'WDQ201  (WDQ2CSEQ>=' W-WDQ2CSEQ-MIN-X                        
255704                    '&WDQ2CSEQ<=' W-WDQ2CSEQ-MAX-X ')'                    
255804          DELIMITED BY SIZE INTO SSA1                                     
255904     MOVE '  GEGB'            TO GODK-STATUSKODER                         
256004     CALL CBLTDLI USING GN WDQ2-PCB DLI-IO-WDQ201 SSA1                    
256440     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
256450     PERFORM IMS-STATUSKONTROLL                                           
256460     .                                                                    
256470     SKIP2                                                                
258505 IMS-GNP-WDQ221          SECTION.                                         
258605     MOVE 'IMS-GNP-WDQ221'   TO CURR-IMS-SECTION                          
258705                                                                          
258811     STRING 'WDQ212  (IDDC     =' W-IDDC-Q2-X ')'                         
259005          DELIMITED BY SIZE INTO SSA1                                     
259111     MOVE   'WDQ221'          TO SSA2                                     
259211     MOVE   '  GE'            TO GODK-STATUSKODER                         
259305     CALL CBLTDLI USING GNP WDQ2-PCB DLI-IO-WDQ221 SSA1 SSA2              
259405     MOVE WDQ2-STATUS-CODE    TO STATUS-WS                                
259505     PERFORM IMS-STATUSKONTROLL                                           
259605     .                                                                    
260705 IMS-STATUSKONTROLL SECTION.                                              
260805                                                                          
260905     SET STATUS-IX TO 1                                                   
261005     SEARCH GODK-STATUS                                                   
261105       AT END                                                             
261205         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
261305         DELIMITED BY SIZE INTO FELTEXT                                   
261405         CALL FELLOG                                                      
261505       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
261605         CONTINUE                                                         
262005     END-SEARCH                                                           
270005     .                                                                    
