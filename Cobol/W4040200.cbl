000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4040200.                                                
000300 AUTHOR.         OLGRENER LASSI.                                          
000400 DATE-WRITTEN.   04/10/21.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        HANTERING AV DC-STYRREGISTER WDB6                                
000900*                                                                         
001000*        PROGRAMMET UPPDATERAR WDB6, WDR6                                 
001100*                                                                         
001200*    INDATA.                                                              
001300*        TRANSAKTION: W4T402                                              
001400*        MID:         W4I40201                                            
001500*                                                                         
001600*    UTDATA.                                                              
001700*        MOD:         W4O40201                                            
001800*                                                                         
001900*                                                                         
002000******************************************************************        
002100*                                                                         
002200* 2011-05-06 SO E'TRACKER 7936414 STOCKING SCREEN IN PULS (2367)          
002300*                                                                         
002400* 2012-05-23 SO E'TRACKER 10151888 SEPARAT LOGGNING BILD 2366 2367        
002500*                                  IDUSER OCH TIUPPDAT PÅ WDB6.           
002600*                                                                         
002700* 2014-01-28 DATTA ARUP          - ENABLE TO START AREA 88                
002800*                                  ORDERS UPTO 5 DAYS.                    
002900*                                  SCR 10200572                           
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300                                                                          
003400 DATA DIVISION.                                                           
003500     EJECT                                                                
003600 WORKING-STORAGE SECTION.                                                 
003700 77  IDPGM                       PIC X(08)   VALUE 'W4040200'.            
003800 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003900                                                                          
004000 77  JA                          PIC X       VALUE 'J'.                   
004100 77  YES                         PIC X       VALUE 'Y'.                   
004200 77  NEJ                         PIC X       VALUE 'N'.                   
004300                                                                          
004400*01  -COPY WWDCKONS                                                       
004500                                                                          
004600 77  WS-TIHHMM-START             PIC 9(4)    VALUE ZERO.                  
004700 77  WS-TIHHMM-READY             PIC 9(4)    VALUE ZERO.                  
004800                                                                          
004900*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005000                                                                          
005100 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005200     88  INDATA-OK                           VALUE 'J'.                   
005300     88  INDATA-FEL                          VALUE 'N'.                   
005400                                                                          
005500 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005600     88  NYCKLAR-OK                          VALUE 'J'.                   
005700     88  NYCKLAR-FEL                         VALUE 'N'.                   
005800                                                                          
005900 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006000     88  EGEN-MID                            VALUE '4402'.                
006100     88  GODK-MID                            VALUE '4402' '0551'.         
006200     88  HELP-MID                            VALUE '0551'.                
006300     EJECT                                                                
006400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006500 01  GENERELLA-SUBPROGRAM.                                                
006600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007000     03  WISOLAND                PIC X(8)    VALUE 'WISOLAND'.            
007100     EJECT                                                                
007200*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007300*01 -COPY WMEDAREA                                                        
007400     SKIP3                                                                
007500 01  MESSAGE-CODES.                                                       
007600     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
007700     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
007800     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
007900     03  ERR-DC-MISSING          PIC X(3)    VALUE '026'.                 
008000     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
008100     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008200     EJECT                                                                
008300*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008400*                                                                         
008500 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008600     SKIP3                                                                
008700*01 -COPY WMSGINIT                                                        
008800     EJECT                                                                
008900*    --- PARAMETRAR TILL SUBPROGRAM WISOLAND                              
009000*                                                                         
009100 01  FILLER                      PIC X(16)   VALUE 'WISOLAND'.            
009200     SKIP3                                                                
009300*01 -COPY WISOLAND                                                        
009400     EJECT                                                                
009500* - - - - - - - - - - - - - - - - - - - - LAND-AREA                       
009600 01  FILLER                      PIC X(16) VALUE 'LAND-AREA'.             
009700*01  -COPY WWLAND06                                                       
009800     EJECT                                                                
009900*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
010000*                                                                         
010100 01  SPAR-AREA.                                                           
010200     03  SPAR-IDTRANS           PIC X(4)    VALUE '4402'.                 
010300     EJECT                                                                
010400*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
010500*                                                                         
010600 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
010700     SKIP3                                                                
010800*01  MID -COPY W4I40201                                                   
010900     EJECT                                                                
011000 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
011100     SKIP3                                                                
011200*01  -COPY WMSGAREA                                                       
011300     EJECT                                                                
011400     03  MOD REDEFINES MSG-AREA.                                          
011500*      05  -COPY W4O40201                                                 
011600     EJECT                                                                
011700 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
011800     SKIP3                                                                
011900*01  -COPY WMFSAREA                                                       
012000     EJECT                                                                
012100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
012200*                                                                         
012300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012400     SKIP3                                                                
012500 01  NYCKLAR-TILL-DLI.                                                    
012600     03  W-IDDC-X.                                                        
012700         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
012800     SKIP2                                                                
012900*    --- STATUS-KOD FRÅN IMS                                              
013000 01  STATUS-WS                   PIC XX.                                  
013100     88  SEGMENT-FINNS                       VALUE '  '.                  
013200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
013300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
013400     SKIP2                                                                
013500 01  GODK-STATUSKODER.                                                    
013600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013700     SKIP3                                                                
013800 01  SSA1                        PIC X(64).                               
013900 01  SSA2                        PIC X(64).                               
014000     EJECT                                                                
014100*    --- IMS FUNKTIONSKODER                                               
014200*01  -COPY W0003                                                          
014300     EJECT                                                                
014400*    ---  DLI INPUT-OUTPUT AREA                                           
014500                                                                          
014600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
014700 01  DLI-IO-WDB601.                                                       
014800*    03  -COPY WDB601                                                     
014900     EJECT                                                                
015000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB602'.                      
015100 01  DLI-IO-WDB602.                                                       
015200*    03  -COPY WDB602                                                     
015300     EJECT                                                                
015400                                                                          
015500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR601'.                      
015600 01  DLI-IO-WDR601.                                                       
015700*    03  -COPY WDR601                                                     
015800*    05  LOGG  -COPY W414DCSA          -RED FIL-WDR601-DATA.              
015900     EJECT                                                                
016000 LINKAGE SECTION.                                                         
016100*01  -COPY W0009   -PRE MSG-                                              
016200*01  -COPY W0008   -PRE WDP7-                                             
016300     05  FILLER                  PIC X.                                   
016400                                                                          
016500*01  -COPY W0008  -PRE WDB6-                                              
016600     05  FILLER                  PIC X.                                   
016700     EJECT                                                                
016800*01  -COPY W0008  -PRE WDR6-                                              
016900     05  FILLER                  PIC X.                                   
017000     EJECT                                                                
017100 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB WDB6-PCB WDR6-PCB.            
017200 MAIN SECTION.                                                            
017300     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDB6-PCB WDR6-PCB.            
017400                                                                          
017500     PERFORM IMS-GET-MSG                                                  
017600                                                                          
017700     IF SEGMENT-FINNS                                                     
017800       PERFORM A-INIT                                                     
017900       PERFORM B-KOLLA-NYCKLAR                                            
018000       IF NYCKLAR-OK                                                      
018100         IF MFS-UPDATE                                                    
018200           PERFORM G-KOLLA-INPUT                                          
018300           IF INDATA-OK                                                   
018400             PERFORM H-UPPDATERA                                          
018500           END-IF                                                         
018600         ELSE                                                             
018700           IF MFS-FIRST                                                   
018800             PERFORM C-FOERSTA-SIDA                                       
018900           ELSE                                                           
019000             PERFORM E-SAMMA-SIDA                                         
019100           END-IF                                                         
019200         END-IF                                                           
019300         IF INDATA-OK                                                     
019400           PERFORM F-LAES-VISA-INFO                                       
019500         END-IF                                                           
019600         IF W-IDDC NOT = '11'                                             
019700           IF  MOD-FLSTOREF-IN-ATTR NOT = MFS-ALFA-FAELT-FEL              
019800           AND MOD-FLSTOREF-IN-ATTR NOT = MFS-ADD-LAES-IN-FAELT           
019900             MOVE MFS-OPEN-ALPHA-NOMOD  TO MOD-FLSTOREF-IN-ATTR           
020000           END-IF                                                         
020100         END-IF                                                           
020200       END-IF                                                             
020300       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O40201 + 4                      
020400       PERFORM IMS-INSERT-MSG                                             
020500     END-IF                                                               
020600                                                                          
020700     MOVE ZERO TO RETURN-CODE                                             
020800     GOBACK                                                               
020900     .                                                                    
021000     EJECT                                                                
021100 A-INIT SECTION.                                                          
021200                                                                          
021300     IF MSG-DUBBLA-TRANSKODER                                             
021400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I40201                 
021500       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
021600       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
021700     ELSE                                                                 
021800       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I40201                  
021900       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
022000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
022100     END-IF                                                               
022200                                                                          
022300     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
022400     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
022500     MOVE MFS-IDTRANS TO W-IDTRANS                                        
022600                                                                          
022700     MOVE LOW-VALUE TO MSG-AREA                                           
022800     MOVE 'W4O402N1' TO MFS-IDMOD                                         
022900     MOVE '4402' TO MOD-IDTRANS                                           
023000     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
023100                                                                          
023200     IF EGEN-MID OR HELP-MID                                              
023300       CONTINUE                                                           
023400     ELSE                                                                 
023500       MOVE SPACE TO MFS-KDTRTYP                                          
023600       MOVE '7' TO MFS-IDPFK                                              
023700     END-IF                                                               
023800     MOVE IDPGM                TO  FIL-IDPGM                              
023900     ACCEPT FIL-TIREGDAT       FROM  DATE                                 
024000     ACCEPT FIL-TIKLOCK        FROM  TIME                                 
024100     MOVE ZERO                 TO  FIL-IDSEKVNR                           
024200     MOVE 'W414'               TO  FIL-CT-IDSYSTEM                        
024300     MOVE 'DCS'                TO  FIL-CT-IDPTYP                          
024400     MOVE 'A'                  TO  FIL-CT-IDVTYP                          
024500     .                                                                    
024600     EJECT                                                                
024700 B-KOLLA-NYCKLAR SECTION.                                                 
024800                                                                          
024900     MOVE ALL '+'           TO MSGI-WMSGINIT                              
025000     MOVE '001'             TO MSGI-KDCALL                                
025100     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
025200     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
025300     MOVE '4402'            TO MSGI-IDTRANS                               
025400*??  IF GODK-MID                                                          
025500         MOVE MID-IDDC-IN     TO MSGI-IDDC-KEY                            
025600*    END-IF                                                               
025700     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
025800     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
025900                                                                          
026000*    - SPRÅK SOM SKA ANVÄNDAS AV WMEDKONV                                 
026100     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
026200                                                                          
026300     MOVE JA TO NYCKLAR-SW                                                
026400                                                                          
026500                                                                          
026600*    -- KONTROLL AV IDDC                                                  
026700     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
026800                                                                          
026900     IF MID-IDDC-IN NOT = ALL '+'                                         
027000       MOVE '7'         TO MFS-IDPFK                                      
027100       MOVE SPACE       TO MFS-KDTRTYP                                    
027200     END-IF                                                               
027300     MOVE MSGI-IDDC-KEY TO W-IDDC                                         
027400                           MOD-IDDC-UT                                    
027500                           DCSL-IDDC                                      
027600                                                                          
027700     MOVE MSGI-IDUSER   TO DCSL-IDUSER                                    
027800     MOVE MSGI-BEANST   TO DCSL-BEANST                                    
027900     MOVE 'OLD VALUE: ' TO DCSL-BETEXT-OLD                                
028000     MOVE 'NEW VALUE: ' TO DCSL-BETEXT-NEW                                
028100     .                                                                    
028200     EJECT                                                                
028300 C-FOERSTA-SIDA SECTION.                                                  
028400                                                                          
028500     PERFORM MFS-RENSA-FAELT-IN                                           
028600     .                                                                    
028700     EJECT                                                                
028800 E-SAMMA-SIDA SECTION.                                                    
028900                                                                          
029000     IF EGEN-MID OR HELP-MID                                              
029100       IF MID-INPUT = ALL '+'                                             
029200         PERFORM MFS-RENSA-FAELT-IN                                       
029300       ELSE                                                               
029400         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
029500         CALL WMEDKONV USING MED-WMEDAREA                                 
029600         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
029700         PERFORM EA-MID-INDATA-TILL-MOD                                   
029800       END-IF                                                             
029900     ELSE                                                                 
030000       PERFORM MFS-RENSA-FAELT-IN                                         
030100     END-IF                                                               
030200     .                                                                    
030300     EJECT                                                                
030400 EA-MID-INDATA-TILL-MOD SECTION.                                          
030500                                                                          
030600     IF MID-FLNYSEG = ALL '+'                                             
030700       MOVE MFS-RENSA-FAELT       TO MOD-FLNYSEG                          
030800     ELSE                                                                 
030900       MOVE MID-FLNYSEG           TO MOD-FLNYSEG                          
031000       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLNYSEG-ATTR                     
031100     END-IF                                                               
031200                                                                          
031300     IF MID-BEGMT-RAD1 = ALL '+'                                          
031400       MOVE MFS-RENSA-FAELT       TO MOD-BEGMT-RAD1                       
031500     ELSE                                                                 
031600       MOVE MID-BEGMT-RAD1        TO MOD-BEGMT-RAD1                       
031700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-BEGMT-RAD1-ATTR                  
031800     END-IF                                                               
031900                                                                          
032000     IF MID-BEGMT-RAD2 = ALL '+'                                          
032100       MOVE MFS-RENSA-FAELT       TO MOD-BEGMT-RAD2                       
032200     ELSE                                                                 
032300       MOVE MID-BEGMT-RAD2        TO MOD-BEGMT-RAD2                       
032400       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-BEGMT-RAD2-ATTR                  
032500     END-IF                                                               
032600                                                                          
032700     IF MID-ADGMT-GATA = ALL '+'                                          
032800       MOVE MFS-RENSA-FAELT       TO MOD-ADGMT-GATA                       
032900     ELSE                                                                 
033000       MOVE MID-ADGMT-GATA        TO MOD-ADGMT-GATA                       
033100       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ADGMT-GATA-ATTR                  
033200     END-IF                                                               
033300                                                                          
033400     IF MID-ADPOSTNR = ALL '+'                                            
033500       MOVE MFS-RENSA-FAELT       TO MOD-ADPOSTNR                         
033600     ELSE                                                                 
033700       MOVE MID-ADPOSTNR          TO MOD-ADPOSTNR                         
033800       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ADPOSTNR-ATTR                    
033900     END-IF                                                               
034000                                                                          
034100     IF MID-ADCITY = ALL '+'                                              
034200       MOVE MFS-RENSA-FAELT       TO MOD-ADCITY                           
034300     ELSE                                                                 
034400       MOVE MID-ADCITY            TO MOD-ADCITY                           
034500       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ADCITY-ATTR                      
034600     END-IF                                                               
034700                                                                          
034800     IF MID-ADGMT-LAND = ALL '+'                                          
034900       MOVE MFS-RENSA-FAELT       TO MOD-ADGMT-LAND                       
035000     ELSE                                                                 
035100       MOVE MID-ADGMT-LAND        TO MOD-ADGMT-LAND                       
035200       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ADGMT-LAND-ATTR                  
035300     END-IF                                                               
035400                                                                          
035500     IF MID-BEGMT-RAD1-INV = ALL '+'                                      
035600       MOVE MFS-RENSA-FAELT       TO MOD-BEGMT-RAD1-INV                   
035700     ELSE                                                                 
035800       MOVE MID-BEGMT-RAD1-INV    TO MOD-BEGMT-RAD1-INV                   
035900       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-BEGMT-RAD1-INV-ATTR              
036000     END-IF                                                               
036100                                                                          
036200     IF MID-BEGMT-RAD2-INV = ALL '+'                                      
036300       MOVE MFS-RENSA-FAELT       TO MOD-BEGMT-RAD2-INV                   
036400     ELSE                                                                 
036500       MOVE MID-BEGMT-RAD2-INV    TO MOD-BEGMT-RAD2-INV                   
036600       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-BEGMT-RAD2-INV-ATTR              
036700     END-IF                                                               
036800                                                                          
036900     IF MID-ADGMT-GATA-INV = ALL '+'                                      
037000       MOVE MFS-RENSA-FAELT       TO MOD-ADGMT-GATA-INV                   
037100     ELSE                                                                 
037200       MOVE MID-ADGMT-GATA-INV    TO MOD-ADGMT-GATA-INV                   
037300       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ADGMT-GATA-INV-ATTR              
037400     END-IF                                                               
037500                                                                          
037600     IF MID-ADPOSTNR-INV = ALL '+'                                        
037700       MOVE MFS-RENSA-FAELT       TO MOD-ADPOSTNR-INV                     
037800     ELSE                                                                 
037900       MOVE MID-ADPOSTNR-INV      TO MOD-ADPOSTNR-INV                     
038000       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ADPOSTNR-INV-ATTR                
038100     END-IF                                                               
038200                                                                          
038300     IF MID-ADCITY-INV = ALL '+'                                          
038400       MOVE MFS-RENSA-FAELT       TO MOD-ADCITY-INV                       
038500     ELSE                                                                 
038600       MOVE MID-ADCITY-INV        TO MOD-ADCITY-INV                       
038700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ADCITY-INV-ATTR                  
038800     END-IF                                                               
038900                                                                          
039000     IF MID-ADGMT-LAND-INV = ALL '+'                                      
039100       MOVE MFS-RENSA-FAELT       TO MOD-ADGMT-LAND-INV                   
039200     ELSE                                                                 
039300       MOVE MID-ADGMT-LAND-INV    TO MOD-ADGMT-LAND-INV                   
039400       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ADGMT-LAND-INV-ATTR              
039500     END-IF                                                               
039600                                                                          
039700     IF MID-KDDC-IN = ALL '+'                                             
039800       MOVE MFS-RENSA-FAELT       TO MOD-KDDC-IN                          
039900     ELSE                                                                 
040000       MOVE MID-KDDC-IN           TO MOD-KDDC-IN                          
040100       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDDC-IN-ATTR                     
040200     END-IF                                                               
040300                                                                          
040400     IF MID-IDLANDX2-IN = ALL '+'                                         
040500       MOVE MFS-RENSA-FAELT       TO MOD-IDLANDX2-IN                      
040600     ELSE                                                                 
040700       MOVE MID-IDLANDX2-IN       TO MOD-IDLANDX2-IN                      
040800       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLANDX2-IN-ATTR                 
040900     END-IF                                                               
041000                                                                          
041100     IF MID-IDLEVNR-IN = ALL '+'                                          
041200       MOVE MFS-RENSA-FAELT       TO MOD-IDLEVNR-IN                       
041300     ELSE                                                                 
041400       MOVE MID-IDLEVNR-IN        TO MOD-IDLEVNR-IN                       
041500       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLEVNR-IN-ATTR                  
041600     END-IF                                                               
041700                                                                          
041800     IF MID-FLWEBDC-IN = ALL '+'                                          
041900       MOVE MFS-RENSA-FAELT       TO MOD-FLWEBDC-IN                       
042000     ELSE                                                                 
042100       IF MID-FLWEBDC-IN = JA                                             
042200          MOVE YES                TO MOD-FLWEBDC-IN                       
042300       ELSE                                                               
042400          MOVE MID-FLWEBDC-IN     TO MOD-FLWEBDC-IN                       
042500       END-IF                                                             
042600       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLWEBDC-IN-ATTR                  
042700     END-IF                                                               
042800                                                                          
042900     IF MID-IDTIDZON-IN = ALL '+'                                         
043000       MOVE MFS-RENSA-FAELT       TO MOD-IDTIDZON-IN                      
043100     ELSE                                                                 
043200       MOVE MID-IDTIDZON-IN       TO MOD-IDTIDZON-IN                      
043300       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDTIDZON-IN-ATTR                 
043400     END-IF                                                               
043500                                                                          
043600     IF MID-KDVALISO-IN = ALL '+'                                         
043700       MOVE MFS-RENSA-FAELT       TO MOD-KDVALISO-IN                      
043800     ELSE                                                                 
043900       MOVE MID-KDVALISO-IN       TO MOD-KDVALISO-IN                      
044000       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDVALISO-IN-ATTR                 
044100     END-IF                                                               
044200                                                                          
044300     IF MID-IDVAT = ALL '+'                                               
044400       MOVE MFS-RENSA-FAELT       TO MOD-IDVAT                            
044500     ELSE                                                                 
044600       MOVE MID-IDVAT             TO MOD-IDVAT                            
044700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDVAT-ATTR                       
044800     END-IF                                                               
044900                                                                          
045000     IF MID-FLINVACS-IN = ALL '+'                                         
045100       MOVE MFS-RENSA-FAELT       TO MOD-FLINVACS-IN                      
045200     ELSE                                                                 
045300       IF MID-FLINVACS-IN = JA                                            
045400          MOVE YES                TO MOD-FLINVACS-IN                      
045500       ELSE                                                               
045600          MOVE MID-FLINVACS-IN    TO MOD-FLINVACS-IN                      
045700       END-IF                                                             
045800       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLINVACS-IN-ATTR                 
045900     END-IF                                                               
046000                                                                          
046100     IF MID-IDFTG-IN = ALL '+'                                            
046200       MOVE MFS-RENSA-FAELT       TO MOD-IDFTG-IN                         
046300     ELSE                                                                 
046400       MOVE MID-IDFTG-IN          TO MOD-IDFTG-IN                         
046500       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDFTG-IN-ATTR                    
046600     END-IF                                                               
046700                                                                          
046800     IF MID-IDLEVNR-EMB-IN = ALL '+'                                      
046900       MOVE MFS-RENSA-FAELT       TO MOD-IDLEVNR-EMB-IN                   
047000     ELSE                                                                 
047100       MOVE MID-IDLEVNR-IN        TO MOD-IDLEVNR-EMB-IN                   
047200       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLEVNR-EMB-IN-ATTR              
047300     END-IF                                                               
047400                                                                          
047500     IF MID-IDPARTNR-IN = ALL '+'                                         
047600       MOVE MFS-RENSA-FAELT       TO MOD-IDPARTNR-IN                      
047700     ELSE                                                                 
047800       MOVE MID-IDPARTNR-IN       TO MOD-IDPARTNR-IN                      
047900       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDPARTNR-IN-ATTR                 
048000     END-IF                                                               
048100                                                                          
048200     IF MID-TIHHMM-START-IN = ALL '+'                                     
048300       MOVE MFS-RENSA-FAELT       TO MOD-TIHHMM-START-IN                  
048400     ELSE                                                                 
048500       MOVE MID-TIHHMM-START-IN   TO MOD-TIHHMM-START-IN                  
048600       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TIHHMM-START-IN-ATTR             
048700     END-IF                                                               
048800                                                                          
048900     IF MID-TIHHMM-READY-IN = ALL '+'                                     
049000       MOVE MFS-RENSA-FAELT       TO MOD-TIHHMM-READY-IN                  
049100     ELSE                                                                 
049200       MOVE MID-TIHHMM-READY-IN   TO MOD-TIHHMM-READY-IN                  
049300       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TIHHMM-READY-IN-ATTR             
049400     END-IF                                                               
049500                                                                          
049600     IF MID-FLMAINDC-IN = ALL '+'                                         
049700       MOVE MFS-RENSA-FAELT       TO MOD-FLMAINDC-IN                      
049800     ELSE                                                                 
049900       IF MID-FLMAINDC-IN = JA                                            
050000          MOVE YES                TO MOD-FLMAINDC-IN                      
050100       ELSE                                                               
050200          MOVE MID-FLMAINDC-IN    TO MOD-FLMAINDC-IN                      
050300       END-IF                                                             
050400       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLMAINDC-IN-ATTR                 
050500     END-IF                                                               
050600                                                                          
050700     IF MID-KDTRADP-IN = ALL '+'                                          
050800       MOVE MFS-RENSA-FAELT       TO MOD-KDTRADP-IN                       
050900     ELSE                                                                 
051000       MOVE MID-KDTRADP-IN        TO MOD-KDTRADP-IN                       
051100       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDTRADP-IN-ATTR                  
051200     END-IF                                                               
051300                                                                          
051400     IF MID-IDLEGSEL-IN = ALL '+'                                         
051500       MOVE MFS-RENSA-FAELT       TO MOD-IDLEGSEL-IN                      
051600     ELSE                                                                 
051700       MOVE MID-IDLEGSEL-IN       TO MOD-IDLEGSEL-IN                      
051800       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLEGSEL-IN-ATTR                 
051900     END-IF                                                               
052000                                                                          
052100     IF MID-FLSTOREF-IN = ALL '+'                                         
052200       MOVE MFS-RENSA-FAELT       TO MOD-FLSTOREF-IN                      
052300     ELSE                                                                 
052400       IF MID-FLSTOREF-IN = JA                                            
052500          MOVE YES                TO MOD-FLSTOREF-IN                      
052600       ELSE                                                               
052700          MOVE MID-FLSTOREF-IN    TO MOD-FLSTOREF-IN                      
052800       END-IF                                                             
052900       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLSTOREF-IN-ATTR                 
053000     END-IF                                                               
053100                                                                          
053200     IF MID-IDSKYLT-IN = ALL '+'                                          
053300       MOVE MFS-RENSA-FAELT       TO MOD-IDSKYLT-IN                       
053400     ELSE                                                                 
053500       MOVE MID-IDSKYLT-IN        TO MOD-IDSKYLT-IN                       
053600       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDSKYLT-IN-ATTR                  
053700     END-IF                                                               
053800                                                                          
053900     IF MID-FLSTOFC-IN = ALL '+'                                          
054000       MOVE MFS-RENSA-FAELT       TO MOD-FLSTOFC-IN                       
054100     ELSE                                                                 
054200       IF MID-FLSTOFC-IN  = JA                                            
054300          MOVE YES                TO MOD-FLSTOFC-IN                       
054400       ELSE                                                               
054500          MOVE MID-FLSTOFC-IN     TO MOD-FLSTOFC-IN                       
054600       END-IF                                                             
054700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLSTOFC-IN-ATTR                  
054800     END-IF                                                               
054900                                                                          
055000     IF MID-FLLPO-IN = ALL '+'                                            
055100       MOVE MFS-RENSA-FAELT       TO MOD-FLLPO-IN                         
055200     ELSE                                                                 
055300       IF MID-FLLPO-IN    = JA                                            
055400          MOVE YES                TO MOD-FLLPO-IN                         
055500       ELSE                                                               
055600          MOVE MID-FLLPO-IN       TO MOD-FLLPO-IN                         
055700       END-IF                                                             
055800       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLLPO-IN-ATTR                    
055900     END-IF                                                               
056000     .                                                                    
056100     EJECT                                                                
056200 F-LAES-VISA-INFO SECTION.                                                
056300                                                                          
056400     PERFORM IMS-GHU-WDB601                                               
056500                                                                          
056600     IF SEGMENT-SAKNAS                                                    
056700       MOVE ERR-DC-MISSING TO MED-IDMFSFEL                                
056800       CALL WMEDKONV USING MED-WMEDAREA                                   
056900       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
057000       PERFORM MFS-RENSA-FAELT-UT                                         
057100     ELSE                                                                 
057200       MOVE DCS-BEGMT-RAD1        TO MOD-BEGMT-RAD1                       
057300       MOVE DCS-BEGMT-RAD2        TO MOD-BEGMT-RAD2                       
057400       MOVE DCS-IDLANDX2          TO MOD-IDLANDX2                         
057500       MOVE DCS-IDLEVNR-DC        TO MOD-IDLEVNR                          
057600       MOVE DCS-KDDC              TO MOD-KDDC                             
057700       IF DCS-FLWEBDC = JA                                                
057800          MOVE YES                TO MOD-FLWEBDC                          
057900       ELSE                                                               
058000          MOVE DCS-FLWEBDC        TO MOD-FLWEBDC                          
058100       END-IF                                                             
058200       MOVE DCS-IDTIDZON          TO MOD-IDTIDZON                         
058300       MOVE DCS-KDVALISO          TO MOD-KDVALISO                         
058400       MOVE DCS-IDVAT             TO MOD-IDVAT                            
058500       IF DCS-FLINVACS = JA                                               
058600          MOVE YES                TO MOD-FLINVACS                         
058700       ELSE                                                               
058800          MOVE DCS-FLINVACS       TO MOD-FLINVACS                         
058900       END-IF                                                             
059000       MOVE DCS-IDFTG             TO MOD-IDFTG                            
059100       MOVE DCS-IDLEVNR-EMB       TO MOD-IDLEVNR-EMB                      
059200       MOVE DCS-IDPARTNR          TO MOD-IDPARTNR                         
059300       MOVE DCS-IDLEGSEL          TO MOD-IDLEGSEL                         
059400       MOVE DCS-KDTRADP           TO MOD-KDTRADP                          
059500       MOVE DCS-ADGMT-GATA        TO MOD-ADGMT-GATA                       
059600       MOVE DCS-ADGMT-PADR(1:10)  TO MOD-ADPOSTNR                         
059700       MOVE DCS-ADGMT-PADR(11:20) TO MOD-ADCITY                           
059800       MOVE DCS-ADGMT-LAND        TO MOD-ADGMT-LAND                       
059900       MOVE DCS-TIHHMM-START      TO MOD-TIHHMM-START                     
060000       MOVE DCS-TIHHMM-READY      TO MOD-TIHHMM-READY                     
060100       IF DCS-FLMAINDC = JA                                               
060200          MOVE YES                TO MOD-FLMAINDC                         
060300       ELSE                                                               
060400          MOVE DCS-FLMAINDC       TO MOD-FLMAINDC                         
060500       END-IF                                                             
060600       IF DCS-FLSTOREF = JA                                               
060700          MOVE YES                TO MOD-FLSTOREF                         
060800       ELSE                                                               
060900          MOVE DCS-FLSTOREF       TO MOD-FLSTOREF                         
061000       END-IF                                                             
061100       MOVE DCS-IDSKYLT-DB        TO MOD-IDSKYLT-DB                       
061200       IF DCS-FLSTOFC = JA                                                
061300          MOVE YES                TO MOD-FLSTOFC                          
061400       ELSE                                                               
061500          MOVE DCS-FLSTOFC        TO MOD-FLSTOFC                          
061600       END-IF                                                             
061700       IF DCS-FLLPO = JA                                                  
061800          MOVE YES                TO MOD-FLLPO                            
061900       ELSE                                                               
062000          MOVE DCS-FLLPO          TO MOD-FLLPO                            
062100       END-IF                                                             
062200                                                                          
062300       PERFORM IMS-GNP-WDB602                                             
062400       IF SEGMENT-FINNS                                                   
062500          MOVE INV-BEGMT-RAD1        TO MOD-BEGMT-RAD1-INV                
062600          MOVE INV-BEGMT-RAD2        TO MOD-BEGMT-RAD2-INV                
062700          MOVE INV-ADGMT-GATA        TO MOD-ADGMT-GATA-INV                
062800          MOVE INV-ADGMT-PADR(1:10)  TO MOD-ADPOSTNR-INV                  
062900          MOVE INV-ADGMT-PADR(11:20) TO MOD-ADCITY-INV                    
063000          MOVE INV-ADGMT-LAND        TO MOD-ADGMT-LAND-INV                
063100       ELSE                                                               
063200          MOVE MFS-RENSA-FAELT       TO MOD-BEGMT-RAD1-INV                
063300                                        MOD-BEGMT-RAD2-INV                
063400                                        MOD-ADGMT-GATA-INV                
063500                                        MOD-ADPOSTNR-INV                  
063600                                        MOD-ADCITY-INV                    
063700                                        MOD-ADGMT-LAND-INV                
063800       END-IF                                                             
063900                                                                          
064000     END-IF                                                               
064100     .                                                                    
064200     EJECT                                                                
064300 G-KOLLA-INPUT SECTION.                                                   
064400                                                                          
064500     MOVE JA  TO INDATA-SW                                                
064600     IF MID-INPUT = ALL '+'                                               
064700       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
064800       CALL WMEDKONV USING MED-WMEDAREA                                   
064900       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
065000       PERFORM MFS-ROER-EJ-FAELT-IN                                       
065100       PERFORM MFS-ROER-EJ-FAELT-UT                                       
065200       MOVE NEJ TO INDATA-SW                                              
065300     ELSE                                                                 
065400       PERFORM IMS-GHU-WDB601                                             
065500       IF SEGMENT-FINNS                                                   
065600         PERFORM GA-KOLLA-ANDR-SEGM                                       
065700       ELSE                                                               
065800         IF MID-FLNYSEG = JA OR YES                                       
065900           PERFORM GB-KOLLA-NY-SEGM                                       
066000         ELSE                                                             
066100           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLNYSEG-ATTR                    
066200           MOVE '?'                TO MOD-FLNYSEG                         
066300           MOVE NEJ TO INDATA-SW                                          
066400         END-IF                                                           
066500       END-IF                                                             
066600                                                                          
066700                                                                          
066800       IF INDATA-FEL                                                      
066900         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
067000         CALL WMEDKONV USING MED-WMEDAREA                                 
067100         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
067200         PERFORM MFS-ROER-EJ-FAELT-UT                                     
067300         PERFORM MFS-ROER-EJ-FAELT-IN                                     
067400       END-IF                                                             
067500     END-IF                                                               
067600     .                                                                    
067700     EJECT                                                                
067800 GA-KOLLA-ANDR-SEGM SECTION.                                              
067900                                                                          
068000     IF MID-BEGMT-RAD1 NOT = ALL '+'                                      
068100       MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEGMT-RAD1-ATTR                   
068200     ELSE                                                                 
068300       MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEGMT-RAD1-ATTR                   
068400     END-IF                                                               
068500                                                                          
068600     IF MID-FLLPO-IN = 'J' OR 'Y'                                         
068700       IF MID-BEGMT-RAD1-INV NOT = ALL '+'                                
068800         IF MID-BEGMT-RAD1-INV NOT = SPACE                                
068900           MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEGMT-RAD1-INV-ATTR           
069000         ELSE                                                             
069100           MOVE MFS-ALFA-FAELT-FEL   TO MOD-BEGMT-RAD1-INV-ATTR           
069200           MOVE NEJ TO INDATA-SW                                          
069300         END-IF                                                           
069400       ELSE                                                               
069500         PERFORM IMS-GNP-WDB602                                           
069600         IF SEGMENT-FINNS                                                 
069700         AND INV-BEGMT-RAD1  NOT = SPACE                                  
069800           MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEGMT-RAD1-INV-ATTR           
069900         ELSE                                                             
070000           MOVE MFS-ALFA-FAELT-FEL   TO MOD-BEGMT-RAD1-INV-ATTR           
070100           MOVE NEJ TO INDATA-SW                                          
070200         END-IF                                                           
070300       END-IF                                                             
070400     ELSE                                                                 
070500       IF  MID-BEGMT-RAD1-INV(1:1)  = SPACE                               
070600       AND MID-ADGMT-GATA-INV(1:1)  = SPACE                               
070700       AND DCS-FLLPO                = 'J'                                 
070800         MOVE 'N'             TO MID-FLLPO-IN                             
070900       ELSE                                                               
071000         IF  MID-BEGMT-RAD1-INV  NOT  = ALL '+'                           
071100         AND MID-BEGMT-RAD1-INV  NOT  = SPACE                             
071200           PERFORM IMS-GNP-WDB602                                         
071300           IF SEGMENT-FINNS                                               
071400             IF INV-BEGMT-RAD1  NOT = MID-BEGMT-RAD1-INV                  
071500               MOVE 'J'             TO MID-FLLPO-IN                       
071600             END-IF                                                       
071700           ELSE                                                           
071800             MOVE 'J'             TO MID-FLLPO-IN                         
071900           END-IF                                                         
072000         END-IF                                                           
072100       END-IF                                                             
072200     END-IF                                                               
072300                                                                          
072400     IF MID-IDLANDX2-IN NOT = ALL '+'                                     
072500        MOVE MID-IDLANDX2-IN TO LAND-IDLANDX2                             
072600        MOVE SPACE           TO LAND-IDLANDX3                             
072700        CALL WISOLAND USING LAND-WISOLAND                                 
072800        IF LAND-KDSVAR = SPACE                                            
072900           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLANDX2-IN-ATTR              
073000        ELSE                                                              
073100           MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDLANDX2-IN-ATTR              
073200           MOVE NEJ TO INDATA-SW                                          
073300        END-IF                                                            
073400     ELSE                                                                 
073500       MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLANDX2-IN-ATTR                  
073600     END-IF                                                               
073700                                                                          
073800     IF MID-IDLEVNR-IN NOT = ALL '+'                                      
073900       IF MID-IDLEVNR-IN = SPACE                                          
074000         MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDLEVNR-IN-ATTR                 
074100         MOVE NEJ TO INDATA-SW                                            
074200       ELSE                                                               
074300         MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLEVNR-IN-ATTR                 
074400       END-IF                                                             
074500                                                                          
074600     ELSE                                                                 
074700       MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLEVNR-IN-ATTR                   
074800     END-IF                                                               
074900                                                                          
075000     IF MID-KDDC-IN NOT = ALL '+'                                         
075100       IF MID-KDDC-IN = 'C ' OR 'TR' OR 'S ' OR 'NC' OR                   
075200                        'NS' OR                                           
075200                        'NA' OR 'NP' OR 'D ' OR 'NX' OR '  '              
075300         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDDC-IN-ATTR                    
075400       ELSE                                                               
075500         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDDC-IN-ATTR                      
075600         MOVE NEJ TO INDATA-SW                                            
075700       END-IF                                                             
075800     ELSE                                                                 
075900       MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDDC-IN-ATTR                      
076000     END-IF                                                               
076100                                                                          
076200     IF MID-IDTIDZON-IN NOT = ALL '+'                                     
076300       IF MID-IDTIDZON-IN NUMERIC AND                                     
076400          MID-IDTIDZON-IN > ZERO                                          
076500         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDTIDZON-IN-ATTR                 
076600       ELSE                                                               
076700         MOVE MFS-NUM-FAELT-FEL TO MOD-IDTIDZON-IN-ATTR                   
076800         MOVE NEJ TO INDATA-SW                                            
076900       END-IF                                                             
077000     END-IF                                                               
077100                                                                          
077200     IF MID-IDVAT NOT = ALL '+'                                           
077300       MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDVAT-ATTR                        
077400     ELSE                                                                 
077500       MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDVAT-ATTR                        
077600     END-IF                                                               
077700                                                                          
077800     IF MID-TIHHMM-START-IN NOT = ALL '+'                                 
077900       IF MID-TIHHMM-START-IN NUMERIC                                     
078000         MOVE MFS-NUM-FAELT-RAETT TO MOD-TIHHMM-START-IN-ATTR             
078100         MOVE MID-TIHHMM-START-IN TO WS-TIHHMM-START                      
078200       ELSE                                                               
078300         MOVE MFS-NUM-FAELT-FEL   TO MOD-TIHHMM-START-IN-ATTR             
078400         MOVE NEJ TO INDATA-SW                                            
078500       END-IF                                                             
078600     ELSE                                                                 
078700        MOVE DCS-TIHHMM-START     TO WS-TIHHMM-START                      
078800     END-IF                                                               
078900                                                                          
079000     IF MID-TIHHMM-READY-IN NOT = ALL '+'                                 
079100       IF MID-TIHHMM-READY-IN NUMERIC                                     
079200         MOVE MFS-NUM-FAELT-RAETT TO MOD-TIHHMM-READY-IN-ATTR             
079300         MOVE MID-TIHHMM-READY-IN TO WS-TIHHMM-READY                      
079400       ELSE                                                               
079500         MOVE MFS-NUM-FAELT-FEL   TO MOD-TIHHMM-READY-IN-ATTR             
079600         MOVE NEJ TO INDATA-SW                                            
079700       END-IF                                                             
079800     ELSE                                                                 
079900        MOVE DCS-TIHHMM-READY     TO WS-TIHHMM-READY                      
080000     END-IF                                                               
080100                                                                          
080200     IF WS-TIHHMM-START > WS-TIHHMM-READY                                 
080300        MOVE MFS-NUM-FAELT-FEL TO MOD-TIHHMM-START-IN-ATTR                
080400        MOVE MFS-NUM-FAELT-FEL TO MOD-TIHHMM-READY-IN-ATTR                
080500        MOVE NEJ TO INDATA-SW                                             
080600     END-IF                                                               
080700                                                                          
080800     IF MID-FLWEBDC-IN NOT = ALL '+'                                      
080900       IF MID-FLWEBDC-IN = 'J' OR 'Y' OR 'N'                              
081000         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLWEBDC-IN-ATTR                 
081100       ELSE                                                               
081200         MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLWEBDC-IN-ATTR                 
081300         MOVE NEJ TO INDATA-SW                                            
081400       END-IF                                                             
081500     END-IF                                                               
081600                                                                          
081700     IF MID-FLINVACS-IN NOT = ALL '+'                                     
081800       IF MID-FLINVACS-IN = 'J' OR 'Y' OR 'N'                             
081900         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLINVACS-IN-ATTR                
082000       ELSE                                                               
082100         MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLINVACS-IN-ATTR                
082200         MOVE NEJ TO INDATA-SW                                            
082300       END-IF                                                             
082400     END-IF                                                               
082500                                                                          
082600     IF MID-IDFTG-IN NOT = ALL '+'                                        
082700       IF MID-IDFTG-IN NUMERIC AND                                        
082800          MID-IDFTG-IN > ZERO                                             
082900         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDFTG-IN-ATTR                    
083000       ELSE                                                               
083100         MOVE MFS-NUM-FAELT-FEL TO MOD-IDFTG-IN-ATTR                      
083200         MOVE NEJ TO INDATA-SW                                            
083300       END-IF                                                             
083400     END-IF                                                               
083500                                                                          
083600     IF MID-IDLEVNR-EMB-IN NOT = ALL '+'                                  
083700       IF MID-IDLEVNR-EMB-IN = SPACE                                      
083800         MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDLEVNR-EMB-IN-ATTR             
083900         MOVE NEJ TO INDATA-SW                                            
084000       ELSE                                                               
084100         MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLEVNR-EMB-IN-ATTR             
084200       END-IF                                                             
084300                                                                          
084400     ELSE                                                                 
084500       MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLEVNR-EMB-IN-ATTR               
084600     END-IF                                                               
084700                                                                          
084800     IF MID-IDPARTNR-IN NOT = ALL '+'                                     
084900       IF MID-IDPARTNR-IN = SPACE                                         
085000         MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDPARTNR-IN-ATTR                
085100         MOVE NEJ TO INDATA-SW                                            
085200       ELSE                                                               
085300         MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPARTNR-IN-ATTR                
085400       END-IF                                                             
085500     ELSE                                                                 
085600       MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPARTNR-IN-ATTR                  
085700     END-IF                                                               
085800                                                                          
085900     IF MID-FLMAINDC-IN NOT = ALL '+'                                     
086000       IF MID-FLMAINDC-IN = 'J' OR 'Y' OR 'N'                             
086100         IF MID-FLMAINDC-IN = 'J' OR 'Y'                                  
086200           PERFORM IMS-GU-WDB601-MAINDC                                   
086300           IF SEGMENT-FINNS                                               
086400             MOVE MFS-ALFA-FAELT-FEL TO MOD-FLMAINDC-IN-ATTR              
086500             MOVE NEJ TO INDATA-SW                                        
086600           END-IF                                                         
086700         ELSE                                                             
086800           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLMAINDC-IN-ATTR              
086900         END-IF                                                           
087000       ELSE                                                               
087100         MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLMAINDC-IN-ATTR                
087200         MOVE NEJ TO INDATA-SW                                            
087300       END-IF                                                             
087400     END-IF                                                               
087500                                                                          
087600     IF MID-KDTRADP-IN NOT = ALL '+'                                      
087700       IF MID-KDTRADP-IN = SPACE                                          
087800         MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDTRADP-IN-ATTR                 
087900         MOVE NEJ                  TO INDATA-SW                           
088000       ELSE                                                               
088100         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDTRADP-IN-ATTR                 
088200       END-IF                                                             
088300     ELSE                                                                 
088400       MOVE MFS-ALFA-FAELT-RAETT   TO MOD-KDTRADP-IN-ATTR                 
088500     END-IF                                                               
088600                                                                          
088700     IF MID-IDLEGSEL-IN NOT = ALL '+'                                     
088800       IF MID-IDLEGSEL-IN = SPACE                                         
088900         MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDLEGSEL-IN-ATTR                
089000         MOVE NEJ                  TO INDATA-SW                           
089100       ELSE                                                               
089200         MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLEGSEL-IN-ATTR                
089300       END-IF                                                             
089400     ELSE                                                                 
089500       MOVE MFS-ALFA-FAELT-RAETT   TO MOD-IDLEGSEL-IN-ATTR                
089600     END-IF                                                               
089700                                                                          
089800     IF MID-FLSTOREF-IN NOT = ALL '+'                                     
089900       IF MID-FLSTOREF-IN = 'J' OR 'Y' OR 'N'                             
090000         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLSTOREF-IN-ATTR                
090100       ELSE                                                               
090200         MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLSTOREF-IN-ATTR                
090300         MOVE NEJ TO INDATA-SW                                            
090400       END-IF                                                             
090500     END-IF                                                               
090600                                                                          
090700     IF MID-IDSKYLT-IN NOT = ALL '+'                                      
090800       IF MID-IDSKYLT-IN = SPACE                                          
090900          MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDSKYLT-IN-ATTR                
091000          MOVE NEJ                  TO INDATA-SW                          
091100       ELSE                                                               
091200          SET WWLAND06-IX TO +1                                           
091300          SEARCH WWLAND06-IDSKYLT-RAD                                     
091400             AT END                                                       
091500                  MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDSKYLT-IN-ATTR        
091600                  MOVE NEJ TO INDATA-SW                                   
091700             WHEN WWLAND06-IDSKYLT(WWLAND06-IX) = MID-IDSKYLT-IN          
091800                  MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDSKYLT-IN-ATTR        
091900          END-SEARCH                                                      
092000       END-IF                                                             
092100     ELSE                                                                 
092200       MOVE MFS-ALFA-FAELT-RAETT   TO MOD-IDSKYLT-IN-ATTR                 
092300     END-IF                                                               
092400                                                                          
092500     IF MID-FLSTOFC-IN NOT = ALL '+'                                      
092600       IF MID-FLSTOFC-IN = 'J' OR 'Y' OR 'N'                              
092700         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLSTOFC-IN-ATTR                 
092800       ELSE                                                               
092900         MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLSTOFC-IN-ATTR                 
093000         MOVE NEJ TO INDATA-SW                                            
093100       END-IF                                                             
093200     END-IF                                                               
093300                                                                          
093400     IF MID-FLLPO-IN NOT = ALL '+'                                        
093500       IF MID-FLLPO-IN = 'J' OR 'Y' OR 'N'                                
093600         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLLPO-IN-ATTR                   
093700       ELSE                                                               
093800         MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLLPO-IN-ATTR                   
093900         MOVE NEJ TO INDATA-SW                                            
094000       END-IF                                                             
094100     END-IF                                                               
094200                                                                          
094300     .                                                                    
094400     EJECT                                                                
094500 GB-KOLLA-NY-SEGM SECTION.                                                
094600                                                                          
094700     IF MID-BEGMT-RAD1 NOT = ALL '+'                                      
094800       MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEGMT-RAD1-ATTR                   
094900       MOVE MID-BEGMT-RAD1       TO DCS-BEGMT-RAD1                        
095000     ELSE                                                                 
095100       MOVE MFS-ALFA-FAELT-FEL   TO MOD-BEGMT-RAD1-ATTR                   
095200       MOVE NEJ TO INDATA-SW                                              
095300     END-IF                                                               
095400                                                                          
095500     IF MID-BEGMT-RAD2 NOT = ALL '+'                                      
095600       MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEGMT-RAD2-ATTR                   
095700       MOVE MID-BEGMT-RAD2       TO DCS-BEGMT-RAD2                        
095800     ELSE                                                                 
095900       MOVE SPACE                TO MOD-BEGMT-RAD2                        
096000                                    DCS-BEGMT-RAD2                        
096100     END-IF                                                               
096200                                                                          
096300     MOVE MID-IDLANDX2-IN TO LAND-IDLANDX2                                
096400     MOVE SPACE           TO LAND-IDLANDX3                                
096500     CALL WISOLAND USING LAND-WISOLAND                                    
096600     IF LAND-KDSVAR = SPACE                                               
096700        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLANDX2-IN-ATTR                 
096800        MOVE MID-IDLANDX2-IN      TO DCS-IDLANDX2                         
096900                                     DCS-IDRT(1:2)                        
097000        MOVE 1                    TO DCS-IDRT(3:1)                        
097100     ELSE                                                                 
097200        MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDLANDX2-IN-ATTR                 
097300        MOVE NEJ TO INDATA-SW                                             
097400     END-IF                                                               
097500                                                                          
097600     IF MID-IDLEVNR-IN NOT = ALL '+' AND                                  
097700        MID-IDLEVNR-IN NOT = SPACE                                        
097800       MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLEVNR-IN-ATTR                   
097900       MOVE MID-IDLEVNR-IN       TO DCS-IDLEVNR-DC                        
098000     ELSE                                                                 
098100       MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDLEVNR-IN-ATTR                   
098200       MOVE NEJ TO INDATA-SW                                              
098300     END-IF                                                               
098400                                                                          
098500     IF MID-KDDC-IN = 'C ' OR 'TR' OR 'S ' OR 'NC' OR                     
098600                      'NS' OR                                             
098600                      'NA' OR 'NP' OR 'D ' OR 'NX' OR '  '                
098700       MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDDC-IN-ATTR                      
098800       MOVE MID-KDDC-IN TO DCS-KDDC                                       
098900     ELSE                                                                 
099000       MOVE MFS-ALFA-FAELT-FEL TO MOD-KDDC-IN-ATTR                        
099100       MOVE NEJ TO INDATA-SW                                              
099200     END-IF                                                               
099300                                                                          
099400     IF MID-ADGMT-GATA NOT = ALL '+'                                      
099500       MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADGMT-GATA-ATTR                   
099600       MOVE MID-ADGMT-GATA       TO DCS-ADGMT-GATA                        
099700     ELSE                                                                 
099800       MOVE SPACE                TO MOD-ADGMT-GATA                        
099900                                    DCS-ADGMT-GATA                        
100000     END-IF                                                               
100100                                                                          
100200     IF MID-ADPOSTNR NOT = ALL '+'                                        
100300       MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADPOSTNR-ATTR                     
100400       MOVE MID-ADPOSTNR         TO DCS-ADGMT-PADR(1:10)                  
100500     ELSE                                                                 
100600       MOVE SPACE                TO MOD-ADPOSTNR                          
100700                                    DCS-ADGMT-PADR(1:10)                  
100800     END-IF                                                               
100900                                                                          
101000     IF MID-ADCITY NOT = ALL '+'                                          
101100       MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADCITY-ATTR                       
101200       MOVE MID-ADCITY           TO DCS-ADGMT-PADR(11:20)                 
101300     ELSE                                                                 
101400       MOVE SPACE                TO MOD-ADCITY                            
101500                                    DCS-ADGMT-PADR(11:20)                 
101600     END-IF                                                               
101700                                                                          
101800     IF MID-ADGMT-LAND NOT = ALL '+'                                      
101900       MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADGMT-LAND-ATTR                   
102000       MOVE MID-ADGMT-LAND       TO DCS-ADGMT-LAND                        
102100     ELSE                                                                 
102200       MOVE SPACE                TO MOD-ADGMT-LAND-ATTR                   
102300                                    DCS-ADGMT-LAND                        
102400     END-IF                                                               
102500                                                                          
102600     IF (MID-FLLPO-IN = 'J' OR 'Y' )                                      
102700     OR MID-BEGMT-RAD1-INV NOT = ALL '+'                                  
102800     OR MID-ADGMT-GATA-INV NOT = ALL '+'                                  
102900       IF MID-BEGMT-RAD1-INV NOT = ALL '+'                                
103000         MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEGMT-RAD1-INV-ATTR             
103100         MOVE MID-BEGMT-RAD1-INV   TO INV-BEGMT-RAD1                      
103200         IF  MID-FLLPO-IN NOT = 'N'                                       
103300         AND (  MID-BEGMT-RAD1-INV NOT = ALL '+'                          
103400             OR MID-ADGMT-GATA-INV NOT = ALL '+')                         
103500             MOVE 'J'             TO MID-FLLPO-IN                         
103600         END-IF                                                           
103700       ELSE                                                               
103800         MOVE MFS-ALFA-FAELT-FEL   TO MOD-BEGMT-RAD1-INV-ATTR             
103900         MOVE NEJ TO INDATA-SW                                            
104000       END-IF                                                             
104100                                                                          
104200       IF MID-BEGMT-RAD2-INV NOT = ALL '+'                                
104300         MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEGMT-RAD2-INV-ATTR             
104400         MOVE MID-BEGMT-RAD2-INV   TO INV-BEGMT-RAD2                      
104500       ELSE                                                               
104600         MOVE SPACE                TO MOD-BEGMT-RAD2-INV                  
104700                                      INV-BEGMT-RAD2                      
104800       END-IF                                                             
104900                                                                          
105000       IF MID-ADGMT-GATA-INV NOT = ALL '+'                                
105100         MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADGMT-GATA-INV-ATTR             
105200         MOVE MID-ADGMT-GATA-INV   TO INV-ADGMT-GATA                      
105300       ELSE                                                               
105400         MOVE SPACE                TO MOD-ADGMT-GATA-INV                  
105500                                      INV-ADGMT-GATA                      
105600       END-IF                                                             
105700                                                                          
105800       IF MID-ADPOSTNR-INV NOT = ALL '+'                                  
105900         MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADPOSTNR-INV-ATTR               
106000         MOVE MID-ADPOSTNR-INV     TO INV-ADGMT-PADR(1:10)                
106100       ELSE                                                               
106200         MOVE SPACE                TO MOD-ADPOSTNR-INV                    
106300                                      INV-ADGMT-PADR(1:10)                
106400       END-IF                                                             
106500                                                                          
106600       IF MID-ADCITY-INV NOT = ALL '+'                                    
106700         MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADCITY-INV-ATTR                 
106800         MOVE MID-ADCITY-INV       TO INV-ADGMT-PADR(11:20)               
106900       ELSE                                                               
107000         MOVE SPACE                TO MOD-ADCITY-INV                      
107100                                      INV-ADGMT-PADR(11:20)               
107200       END-IF                                                             
107300                                                                          
107400       IF MID-ADGMT-LAND-INV NOT = ALL '+'                                
107500         MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADGMT-LAND-INV-ATTR             
107600         MOVE MID-ADGMT-LAND-INV   TO INV-ADGMT-LAND                      
107700       ELSE                                                               
107800         MOVE SPACE                TO MOD-ADGMT-LAND-INV-ATTR             
107900                                      INV-ADGMT-LAND                      
108000       END-IF                                                             
108100     END-IF                                                               
108200                                                                          
108300     IF MID-IDTIDZON-IN NUMERIC AND                                       
108400        MID-IDTIDZON-IN > ZERO                                            
108500       MOVE MFS-NUM-FAELT-RAETT TO MOD-IDTIDZON-IN-ATTR                   
108600       MOVE MID-IDTIDZON-IN     TO DCS-IDTIDZON                           
108700     ELSE                                                                 
108800       MOVE MFS-NUM-FAELT-FEL TO MOD-IDTIDZON-IN-ATTR                     
108900       MOVE NEJ TO INDATA-SW                                              
109000     END-IF                                                               
109100                                                                          
109200     IF MID-TIHHMM-START-IN NUMERIC AND                                   
109300        MID-TIHHMM-START-IN > ZERO                                        
109400        MOVE MFS-NUM-FAELT-RAETT TO MOD-TIHHMM-START-IN-ATTR              
109500        MOVE MID-TIHHMM-START-IN TO DCS-TIHHMM-START                      
109600     ELSE                                                                 
109700        MOVE MFS-NUM-FAELT-FEL   TO MOD-TIHHMM-START-IN-ATTR              
109800        MOVE NEJ TO INDATA-SW                                             
109900     END-IF                                                               
110000                                                                          
110100     IF MID-TIHHMM-READY-IN NUMERIC AND                                   
110200        MID-TIHHMM-READY-IN > ZERO                                        
110300        MOVE MFS-NUM-FAELT-RAETT TO MOD-TIHHMM-READY-IN-ATTR              
110400        MOVE MID-TIHHMM-READY-IN TO DCS-TIHHMM-READY                      
110500     ELSE                                                                 
110600        MOVE MFS-NUM-FAELT-FEL   TO MOD-TIHHMM-READY-IN-ATTR              
110700        MOVE NEJ TO INDATA-SW                                             
110800     END-IF                                                               
110900                                                                          
111000     IF MID-KDVALISO-IN = ALL '+'                                         
111100       MOVE MFS-RENSA-FAELT       TO MOD-KDVALISO-IN                      
111200       MOVE SPACE                 TO DCS-KDVALISO                         
111300     ELSE                                                                 
111400       MOVE MID-KDVALISO-IN       TO MOD-KDVALISO-IN                      
111500                                     DCS-KDVALISO                         
111600     END-IF                                                               
111700                                                                          
111800     IF MID-IDVAT = ALL '+'                                               
111900       MOVE MFS-RENSA-FAELT       TO MOD-IDVAT                            
112000       MOVE SPACE                 TO DCS-IDVAT                            
112100     ELSE                                                                 
112200       MOVE MID-IDVAT             TO MOD-IDVAT                            
112300                                     DCS-IDVAT                            
112400     END-IF                                                               
112500                                                                          
112600     IF MID-FLWEBDC-IN NOT = ALL '+'                                      
112700       IF MID-FLWEBDC-IN = 'J' OR 'Y' OR 'N'                              
112800         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLWEBDC-IN-ATTR                 
112900         IF MID-FLWEBDC-IN = 'Y'                                          
113000            MOVE 'J'               TO DCS-FLWEBDC                         
113100         ELSE                                                             
113200            MOVE MID-FLWEBDC-IN    TO DCS-FLWEBDC                         
113300         END-IF                                                           
113400       ELSE                                                               
113500         MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLWEBDC-IN-ATTR                 
113600         MOVE NEJ TO INDATA-SW                                            
113700       END-IF                                                             
113800     END-IF                                                               
113900                                                                          
114000     IF MID-FLINVACS-IN NOT = ALL '+'                                     
114100       IF MID-FLINVACS-IN = 'J' OR 'Y' OR 'N'                             
114200         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLINVACS-IN-ATTR                
114300         IF MID-FLINVACS-IN = 'Y'                                         
114400            MOVE 'J'               TO DCS-FLINVACS                        
114500         ELSE                                                             
114600            MOVE MID-FLINVACS-IN    TO DCS-FLINVACS                       
114700         END-IF                                                           
114800       ELSE                                                               
114900         MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLINVACS-IN-ATTR                
115000         MOVE NEJ TO INDATA-SW                                            
115100       END-IF                                                             
115200     END-IF                                                               
115300                                                                          
115400     IF MID-IDFTG-IN NUMERIC AND                                          
115500        MID-IDFTG-IN > ZERO                                               
115600       MOVE MFS-NUM-FAELT-RAETT TO MOD-IDFTG-IN-ATTR                      
115700       MOVE MID-IDFTG-IN        TO DCS-IDFTG                              
115800     ELSE                                                                 
115900       MOVE MFS-NUM-FAELT-FEL   TO MOD-IDFTG-IN-ATTR                      
116000       MOVE NEJ TO INDATA-SW                                              
116100     END-IF                                                               
116200                                                                          
116300     IF MID-IDLEVNR-EMB-IN NOT = ALL '+' AND                              
116400        MID-IDLEVNR-EMB-IN NOT = SPACE                                    
116500       MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLEVNR-EMB-IN-ATTR               
116600       MOVE MID-IDLEVNR-EMB-IN   TO DCS-IDLEVNR-EMB                       
116700     ELSE                                                                 
116800       MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDLEVNR-EMB-IN-ATTR               
116900       MOVE NEJ TO INDATA-SW                                              
117000     END-IF                                                               
117100                                                                          
117200     IF MID-IDPARTNR-IN NOT = ALL '+' AND                                 
117300        MID-IDPARTNR-IN NOT = SPACE                                       
117400       MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPARTNR-IN-ATTR                  
117500       MOVE MID-IDPARTNR-IN      TO DCS-IDPARTNR                          
117600     ELSE                                                                 
117700       MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDPARTNR-IN-ATTR                  
117800       MOVE NEJ TO INDATA-SW                                              
117900     END-IF                                                               
118000                                                                          
118100     IF MID-FLMAINDC-IN NOT = ALL '+'                                     
118200       IF MID-FLMAINDC-IN = 'N'                                           
118300         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLMAINDC-IN-ATTR                
118400         MOVE MID-FLMAINDC-IN       TO DCS-FLMAINDC                       
118500       ELSE                                                               
118600         MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLMAINDC-IN-ATTR                
118700         MOVE NEJ TO INDATA-SW                                            
118800       END-IF                                                             
118900     END-IF                                                               
119000                                                                          
119100     IF MID-KDTRADP-IN NOT = ALL '+' AND                                  
119200        MID-KDTRADP-IN NOT = SPACE                                        
119300       MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDTRADP-IN-ATTR                   
119400       MOVE MID-KDTRADP-IN       TO DCS-KDTRADP                           
119500     ELSE                                                                 
119600       MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDTRADP-IN-ATTR                   
119700       MOVE NEJ                  TO INDATA-SW                             
119800     END-IF                                                               
119900                                                                          
120000     IF MID-IDLEGSEL-IN NOT = ALL '+' AND                                 
120100        MID-IDLEGSEL-IN NOT = SPACE                                       
120200       MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLEGSEL-IN-ATTR                  
120300       MOVE MID-IDLEGSEL-IN      TO DCS-IDLEGSEL                          
120400     ELSE                                                                 
120500       MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDLEGSEL-IN-ATTR                  
120600       MOVE NEJ TO INDATA-SW                                              
120700     END-IF                                                               
120800                                                                          
120900     IF MID-FLSTOREF-IN NOT = ALL '+'                                     
121000       IF MID-FLSTOREF-IN = 'J' OR 'Y' OR 'N'                             
121100         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLSTOREF-IN-ATTR                
121200         IF MID-FLSTOREF-IN = 'Y'                                         
121300            MOVE 'J'               TO DCS-FLSTOREF                        
121400         ELSE                                                             
121500            MOVE MID-FLSTOREF-IN   TO DCS-FLSTOREF                        
121600         END-IF                                                           
121700       ELSE                                                               
121800         MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLSTOREF-IN-ATTR                
121900         MOVE NEJ TO INDATA-SW                                            
122000       END-IF                                                             
122100     END-IF                                                               
122200                                                                          
122300     IF MID-FLSTOFC-IN NOT = ALL '+'                                      
122400       IF MID-FLSTOFC-IN = 'J' OR 'Y' OR 'N'                              
122500         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLSTOFC-IN-ATTR                 
122600         IF MID-FLSTOFC-IN = 'Y'                                          
122700            MOVE 'J'               TO DCS-FLSTOFC                         
122800         ELSE                                                             
122900            MOVE MID-FLSTOFC-IN    TO DCS-FLSTOFC                         
123000         END-IF                                                           
123100       ELSE                                                               
123200         MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLSTOFC-IN-ATTR                 
123300         MOVE NEJ TO INDATA-SW                                            
123400       END-IF                                                             
123500     END-IF                                                               
123600                                                                          
123700     IF MID-IDSKYLT-IN NOT = ALL '+'                                      
123800       MOVE MID-IDSKYLT-IN         TO DCS-IDSKYLT-DB                      
123900       SET WWLAND06-IX TO +1                                              
124000       SEARCH WWLAND06-IDSKYLT-RAD                                        
124100          AT END                                                          
124200               MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDSKYLT-IN-ATTR           
124300               MOVE NEJ TO INDATA-SW                                      
124400          WHEN WWLAND06-IDSKYLT(WWLAND06-IX) = DCS-IDSKYLT-DB             
124500               MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDSKYLT-IN-ATTR           
124600       END-SEARCH                                                         
124700     END-IF                                                               
124800                                                                          
124900     IF MID-FLLPO-IN NOT = ALL '+'                                        
125000       IF MID-FLLPO-IN = 'J' OR 'Y' OR 'N'                                
125100         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLLPO-IN-ATTR                   
125200         IF MID-FLLPO-IN = 'Y'                                            
125300            MOVE 'J'               TO DCS-FLLPO                           
125400         ELSE                                                             
125500            MOVE MID-FLLPO-IN      TO DCS-FLLPO                           
125600         END-IF                                                           
125700       ELSE                                                               
125800         MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLLPO-IN-ATTR                   
125900         MOVE NEJ TO INDATA-SW                                            
126000       END-IF                                                             
126100     END-IF                                                               
126200                                                                          
126300     .                                                                    
126400     EJECT                                                                
126500 H-UPPDATERA SECTION.                                                     
126600                                                                          
126700     PERFORM IMS-GHU-WDB601                                               
126800     IF SEGMENT-FINNS                                                     
126900       PERFORM HA-AENDRA-SEGM                                             
127000     ELSE                                                                 
127100       MOVE MSGI-IDDC-KEY TO DCS-IDDC                                     
127200       MOVE WC-CDC-SE     TO DCS-IDDC-REF                                 
127300       MOVE 71            TO DCS-IDDISTR-JUST                             
127400       MOVE 90            TO DCS-IDDISTR-MIX                              
127500       MOVE 90            TO DCS-IDDISTR-SKROT                            
127600       MOVE YES           TO DCS-FLOVRLAGBER                              
127700       MOVE NEJ           TO DCS-FLCLEAR-BULK                             
127800                             DCS-FLDCRET                                  
127900                             DCS-FLEXCP1-PRIO                             
128100                             DCS-FLEXCP1-REFBER                           
128200                             DCS-FLEXCP2-REFBER                           
128300                             DCS-FLINLHIST                                
128400                             DCS-FLFSEDEL                                 
128500                             DCS-FLINLREP                                 
128600                             DCS-FLPRISSPR                                
128700                             DCS-FLSAMPAK                                 
128800                             DCS-FLSEASBER                                
128900                             DCS-FLTYP6JU                                 
129000                             DCS-FLRSI                                    
129100                             DCS-FLBINNUT                                 
129200                             DCS-FLKNDVAL                                 
129300                             DCS-FLARTDC                                  
129400                             DCS-FLTRANS-PAS                              
129500                             DCS-FLTRANS-ERS                              
129600                             DCS-FLRETUR-PAS                              
129700                             DCS-FLSKROT-PAS                              
129800                             DCS-FLMAINDC                                 
129900                             DCS-FLSTOREF                                 
130000*                            DCS-FLSTOFC                                  
130100                             DCS-FLTRACK                                  
130200*                            DCS-FLWEBDC                                  
130300                             DCS-FLSTOREF                                 
130400                             DCS-FLLPO                                    
130500       MOVE ZERO          TO DCS-KDAKDISP-DAG                             
130600                             DCS-KDAKDISP-BULK                            
130700                             DCS-KDFRAKT-BPS                              
130800                             DCS-KDFRAKT-SBPS                             
130900                             DCS-KDFAKTDC                                 
131300                             DCS-IDDISTR-REFILL                           
131400                             DCS-IDDISTR-RETUR                            
131500                             DCS-IDDISTR-QRETUR                           
131600                             DCS-IDDISTR-QSKROT                           
131700                             DCS-IDDISTR-RSKROT                           
131800                             DCS-IDKUNDNR-BPS                             
131900                             DCS-IDKUNDNR-SBPS                            
132000                             DCS-IDKUNDNR-SORD                            
132100                             DCS-IDKUNDNR-RETUR                           
132200                             DCS-IDKUNDNR-SRETUR                          
132300                             DCS-IDKUNDNR-TRETUR                          
132400                             DCS-IDKUNDNR-QRETUR                          
132500                             DCS-IDKUNDNR-SQRET                           
132600                             DCS-IDKUNDNR-SKROT                           
132700                             DCS-IDKUNDNR-QSKROT                          
132800                             DCS-IDKUNDNR-RSKROT                          
132900                             DCS-IDKUNDNR-JUST                            
133000                             DCS-IDKUNDNR-MIX                             
133100                             DCS-IDPERSON-REM                             
133200                             DCS-IDKONTO-JUST                             
133300                             DCS-IDKONTO-MIX                              
133400                             DCS-IDKONTO-SKROT                            
133500                             DCS-TID-RETOS                                
133600                             DCS-TID-RET98                                
133700                             DCS-FLTRLO88-MAN                             
133800                             DCS-FLTRLO88-TIS                             
133900                             DCS-FLTRLO88-ONS                             
134000                             DCS-FLTRLO88-TOR                             
134100                             DCS-FLTRLO88-FRE                             
134200                             DCS-KVPB-LIM                                 
134300                             DCS-KVPERIOD-TPAS                            
134400                             DCS-KVSKROT-SPAS                             
134500                             DCS-KVVECKOR-SPAS                            
134600                             DCS-PRARTSTD-SPAS                            
134700                             DCS-ADLAGOMR-SPAS                            
134800                             DCS-IDPERSON-SPAS                            
134900                             DCS-KDPRODSL-SPAS                            
135000                             DCS-KVDAGAR-POKS                             
135100                             DCS-KVVECKOR-RPAS                            
135200                             DCS-KVVECKOR-TPAS                            
135300                             DCS-SUARTMIN-RPAS                            
135400                             DCS-SUARTMIN-TPAS                            
135500                             DCS-SUVARLIM-TPAS                            
135600                             DCS-TID-TRS                                  
135700                             DCS-TIVV                                     
135800       MOVE ZERO          TO DCS-IDPRTLST-INL                             
135900                             DCS-IDPRTLST-INLA                            
136000                             DCS-IDPRTLST-INVA                            
136100                             DCS-IDPRTLST-INVAB                           
136200                             DCS-IDLISTNR                                 
136300*                            DCS-IDTIDZON                                 
136400       MOVE ZERO          TO DCS-KDDCSTYR-BUY                             
136500                             DCS-KDDCSTYR-KUND                            
136600                             DCS-KDDCSTYR-REFTAB                          
136700                             DCS-KDPORDL                                  
136800                             DCS-KDSKRMET                                 
136900                             DCS-KVINVAUT                                 
137000                             DCS-SUINVGRANS                               
137100                             DCS-REQXBRYT                                 
137200                             DCS-REWILSON                                 
137300*                            DCS-IDLEVNR-DC                               
137400*                            DCS-TIHHMM-START                             
137500*                            DCS-TIHHMM-READY                             
137600       MOVE ZERO          TO DCS-KDRT-MIX                                 
137700                             DCS-KDRT-JUST                                
137800                             DCS-KDRT-SKROT                               
137900                             DCS-KVDAGAR-CROSS                            
138000                             DCS-KVDAGAR-PP                               
138100                             DCS-KVVECKOR-BIN                             
138200                             DCS-PRARTSTD-SKRLO98                         
138300                             DCS-KVPB-LIM-LF                              
138400                             DCS-KVPB-LIM-HF                              
138500                             DCS-KVOT                                     
138600                             DCS-KVVECKOR-FTL                             
138700                             DCS-KVVECKOR-FTM                             
138800                             DCS-KVVECKOR-FTH                             
138900                             DCS-KVOT-RULL12HF                            
139000       MOVE SPACE         TO DCS-IDANALYS-JUST                            
139100                             DCS-IDANALYS-MIX                             
139200                             DCS-IDANALYS-SKROT                           
139300                             DCS-IDDC-TPAS-1                              
139400                             DCS-IDDC-TPAS-2                              
139500                             DCS-IDDC-TPAS-3                              
139600                             DCS-IDTECKEN-SPAS                            
139700                             DCS-IDKST-JUST                               
139800                             DCS-IDKST-MIX                                
139900                             DCS-IDKST-SKROT                              
140000*                            DCS-IDLEVNR-EMB                              
140001*INITIALIZED VALUES FOR AUTO REFILL LOGIC                                 
140010       MOVE '3'           TO DCS-FLEXCP1-REFBEO                           
140020       MOVE '7'           TO DCS-FLEXCP2-REFBEO                           
140030       MOVE 'N'           TO DCS-FLEXCP3-REFBEO                           
140040       MOVE '1'           TO DCS-FLEXCP4-REFBEO                           
140050*                                                                         
140100       MOVE MSGI-IDUSER   TO DCS-IDUSER                                   
140200       ACCEPT DCS-TIUPPDAT   FROM  DATE                                   
140300                                                                          
140400       PERFORM IMS-ISRT-WDB601                                            
140500                                                                          
140600       MOVE 'WDB601'          TO DCSL-BETEXT-ITEM                         
140700       MOVE 'NYTT DC-SEGMENT' TO DCSL-BETEXT-OLDDATA                      
140800       MOVE DCS-IDDC          TO DCSL-BETEXT-NEWDATA                      
140900                                                                          
141000       ADD +1                 TO  FIL-IDSEKVNR                            
141100       PERFORM IMS-ISRT-WDR601                                            
141200                                                                          
141300       IF MID-BEGMT-RAD1-INV NOT = ALL '+'                                
141400         IF MID-BEGMT-RAD1-INV NOT = ALL '+'                              
141500           MOVE MID-BEGMT-RAD1-INV   TO INV-BEGMT-RAD1                    
141600         END-IF                                                           
141700         IF MID-BEGMT-RAD2-INV NOT = ALL '+'                              
141800           MOVE MID-BEGMT-RAD2-INV   TO INV-BEGMT-RAD2                    
141900         END-IF                                                           
142000         IF MID-ADGMT-GATA-INV NOT = ALL '+'                              
142100           MOVE MID-ADGMT-GATA-INV   TO INV-ADGMT-GATA                    
142200         END-IF                                                           
142300         IF MID-ADPOSTNR-INV NOT = ALL '+'                                
142400           MOVE MID-ADPOSTNR-INV     TO INV-ADGMT-PADR(1:10)              
142500         END-IF                                                           
142600         IF MID-ADCITY-INV NOT = ALL '+'                                  
142700           MOVE MID-ADCITY-INV       TO INV-ADGMT-PADR(11:20)             
142800         END-IF                                                           
142900         IF MID-ADGMT-LAND-INV NOT = ALL '+'                              
143000           MOVE MID-ADGMT-LAND-INV   TO INV-ADGMT-LAND                    
143100         END-IF                                                           
143200         PERFORM IMS-ISRT-WDB602                                          
143300                                                                          
143400         MOVE 'WDB602'          TO DCSL-BETEXT-ITEM                       
143500         MOVE 'NYTT DC-SEGMENT' TO DCSL-BETEXT-OLDDATA                    
143600         MOVE DCS-IDDC          TO DCSL-BETEXT-NEWDATA                    
143700                                                                          
143800         ADD +1                 TO  FIL-IDSEKVNR                          
143900         PERFORM IMS-ISRT-WDR601                                          
144000       END-IF                                                             
144100     END-IF                                                               
144200                                                                          
144300     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
144400     CALL WMEDKONV USING MED-WMEDAREA                                     
144500     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
144600     PERFORM MFS-FORM-ATTR                                                
144700     PERFORM MFS-RENSA-FAELT-IN                                           
144800     .                                                                    
144900     EJECT                                                                
145000 HA-AENDRA-SEGM SECTION.                                                  
145100                                                                          
145200     IF MID-BEGMT-RAD1 NOT = ALL '+'                                      
145300       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-BEGMT-RAD1-ATTR                  
145400       MOVE 'BEGMT-RAD1'      TO DCSL-BETEXT-ITEM                         
145500       MOVE DCS-BEGMT-RAD1    TO DCSL-BETEXT-OLDDATA                      
145600       MOVE MID-BEGMT-RAD1    TO DCS-BEGMT-RAD1                           
145700                                 DCSL-BETEXT-NEWDATA                      
145800       ADD +1                 TO FIL-IDSEKVNR                             
145900       PERFORM IMS-ISRT-WDR601                                            
146000     END-IF                                                               
146100                                                                          
146200     IF MID-BEGMT-RAD2    NOT = ALL '+'                                   
146300       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-BEGMT-RAD2-ATTR                  
146400       MOVE 'BEGMT-RAD2'      TO DCSL-BETEXT-ITEM                         
146500       MOVE DCS-BEGMT-RAD2    TO DCSL-BETEXT-OLDDATA                      
146600       MOVE MID-BEGMT-RAD2    TO DCS-BEGMT-RAD2                           
146700                                 DCSL-BETEXT-NEWDATA                      
146800       ADD +1                 TO FIL-IDSEKVNR                             
146900       PERFORM IMS-ISRT-WDR601                                            
147000     END-IF                                                               
147100                                                                          
147200     IF MID-IDLANDX2-IN NOT = ALL '+'                                     
147300       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDLANDX2-IN-ATTR                 
147400       MOVE 'IDLANDX2'        TO DCSL-BETEXT-ITEM                         
147500       MOVE DCS-IDLANDX2      TO DCSL-BETEXT-OLDDATA                      
147600       MOVE MID-IDLANDX2-IN   TO DCS-IDLANDX2                             
147700                                 DCSL-BETEXT-NEWDATA                      
147800                                 DCS-IDRT(1:2)                            
147900       MOVE 1                 TO DCS-IDRT(3:1)                            
148000       ADD +1                 TO FIL-IDSEKVNR                             
148100       PERFORM IMS-ISRT-WDR601                                            
148200     END-IF                                                               
148300                                                                          
148400     IF MID-IDLEVNR-IN NOT = ALL '+'                                      
148500       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDLEVNR-IN-ATTR                  
148600       MOVE 'IDLEVNR'         TO DCSL-BETEXT-ITEM                         
148700       MOVE DCS-IDLEVNR-DC    TO DCSL-BETEXT-OLDDATA                      
148800       MOVE MID-IDLEVNR-IN    TO DCS-IDLEVNR-DC                           
148900                                 DCSL-BETEXT-NEWDATA                      
149000       ADD +1                 TO FIL-IDSEKVNR                             
149100       PERFORM IMS-ISRT-WDR601                                            
149200     END-IF                                                               
149300                                                                          
149400     IF MID-KDDC-IN NOT = ALL '+'                                         
149500       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDDC-IN-ATTR                     
149600       MOVE 'KDDC'            TO DCSL-BETEXT-ITEM                         
149700       MOVE DCS-KDDC          TO DCSL-BETEXT-OLDDATA                      
149800       MOVE MID-KDDC-IN       TO DCS-KDDC                                 
149900                                 DCSL-BETEXT-NEWDATA                      
150000       ADD +1                 TO FIL-IDSEKVNR                             
150100       PERFORM IMS-ISRT-WDR601                                            
150200     END-IF                                                               
150300                                                                          
150400     IF MID-FLWEBDC-IN NOT = ALL '+'                                      
150500       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLWEBDC-IN-ATTR                  
150600       MOVE 'FLWEBDC'         TO DCSL-BETEXT-ITEM                         
150700       MOVE DCS-FLWEBDC       TO DCSL-BETEXT-OLDDATA                      
150800       IF MID-FLWEBDC-IN = YES                                            
150900          MOVE JA             TO DCS-FLWEBDC                              
151000                                 DCSL-BETEXT-NEWDATA                      
151100       ELSE                                                               
151200          MOVE MID-FLWEBDC-IN TO DCS-FLWEBDC                              
151300                                 DCSL-BETEXT-NEWDATA                      
151400       END-IF                                                             
151500       ADD +1                 TO FIL-IDSEKVNR                             
151600       PERFORM IMS-ISRT-WDR601                                            
151700     END-IF                                                               
151800                                                                          
151900     IF MID-IDTIDZON-IN NOT = ALL '+'                                     
152000       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDTIDZON-IN-ATTR                 
152100       MOVE 'IDTIDZON'        TO DCSL-BETEXT-ITEM                         
152200       MOVE DCS-IDTIDZON      TO DCSL-BETEXT-OLDDATA                      
152300       MOVE MID-IDTIDZON-IN   TO DCS-IDTIDZON                             
152400                                 DCSL-BETEXT-NEWDATA                      
152500       ADD +1                 TO FIL-IDSEKVNR                             
152600       PERFORM IMS-ISRT-WDR601                                            
152700     END-IF                                                               
152800                                                                          
152900     IF MID-KDVALISO-IN NOT = ALL '+'                                     
153000       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDVALISO-IN-ATTR                 
153100       MOVE 'KDVALISO'        TO DCSL-BETEXT-ITEM                         
153200       MOVE DCS-KDVALISO      TO DCSL-BETEXT-OLDDATA                      
153300       MOVE MID-KDVALISO-IN   TO DCS-KDVALISO                             
153400                                 DCSL-BETEXT-NEWDATA                      
153500       ADD +1                 TO FIL-IDSEKVNR                             
153600       PERFORM IMS-ISRT-WDR601                                            
153700     END-IF                                                               
153800                                                                          
153900     IF MID-IDVAT NOT = ALL '+'                                           
154000       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDVAT-ATTR                       
154100       MOVE 'IDVAT'           TO DCSL-BETEXT-ITEM                         
154200       MOVE DCS-IDVAT         TO DCSL-BETEXT-OLDDATA                      
154300       MOVE MID-IDVAT         TO DCS-IDVAT                                
154400                                 DCSL-BETEXT-NEWDATA                      
154500       ADD +1                 TO FIL-IDSEKVNR                             
154600       PERFORM IMS-ISRT-WDR601                                            
154700     END-IF                                                               
154800                                                                          
154900     IF MID-ADGMT-GATA NOT = ALL '+'                                      
155000       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-ADGMT-GATA-ATTR                  
155100       MOVE 'ADGMT-GATA'      TO DCSL-BETEXT-ITEM                         
155200       MOVE DCS-ADGMT-GATA    TO DCSL-BETEXT-OLDDATA                      
155300       MOVE MID-ADGMT-GATA    TO DCS-ADGMT-GATA                           
155400                                 DCSL-BETEXT-NEWDATA                      
155500       ADD +1                 TO FIL-IDSEKVNR                             
155600       PERFORM IMS-ISRT-WDR601                                            
155700     END-IF                                                               
155800                                                                          
155900     IF MID-ADPOSTNR NOT = ALL '+'                                        
156000     OR MID-ADCITY   NOT = ALL '+'                                        
156100       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-ADPOSTNR-ATTR                    
156200       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-ADCITY-ATTR                      
156300       MOVE 'ADGMT-PADR'      TO DCSL-BETEXT-ITEM                         
156400       MOVE DCS-ADGMT-PADR    TO DCSL-BETEXT-OLDDATA                      
156500       IF MID-ADPOSTNR NOT = ALL '+'                                      
156600         MOVE MID-ADPOSTNR      TO DCS-ADGMT-PADR(1:10)                   
156700       END-IF                                                             
156800       IF MID-ADCITY   NOT = ALL '+'                                      
156900         MOVE MID-ADCITY        TO DCS-ADGMT-PADR(11:20)                  
157000       END-IF                                                             
157100       MOVE DCS-ADGMT-PADR    TO DCSL-BETEXT-NEWDATA                      
157200       ADD +1                 TO FIL-IDSEKVNR                             
157300       PERFORM IMS-ISRT-WDR601                                            
157400     END-IF                                                               
157500                                                                          
157600     IF MID-ADGMT-LAND NOT = ALL '+'                                      
157700       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-ADGMT-LAND-ATTR                  
157800       MOVE 'ADGMT-LAND'      TO DCSL-BETEXT-ITEM                         
157900       MOVE DCS-ADGMT-LAND    TO DCSL-BETEXT-OLDDATA                      
158000       MOVE MID-ADGMT-LAND    TO DCS-ADGMT-LAND                           
158100                                 DCSL-BETEXT-NEWDATA                      
158200       ADD +1                 TO FIL-IDSEKVNR                             
158300       PERFORM IMS-ISRT-WDR601                                            
158400     END-IF                                                               
158500                                                                          
158600     IF MID-TIHHMM-START-IN NOT = ALL '+'                                 
158700       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TIHHMM-START-IN-ATTR             
158800       MOVE 'TIHHMM-START'        TO DCSL-BETEXT-ITEM                     
158900       MOVE DCS-TIHHMM-START      TO DCSL-BETEXT-OLDDATA                  
159000       MOVE MID-TIHHMM-START-IN   TO DCS-TIHHMM-START                     
159100                                     DCSL-BETEXT-NEWDATA                  
159200       ADD +1                     TO FIL-IDSEKVNR                         
159300       PERFORM IMS-ISRT-WDR601                                            
159400     END-IF                                                               
159500                                                                          
159600     IF MID-TIHHMM-READY-IN NOT = ALL '+'                                 
159700       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TIHHMM-READY-IN-ATTR             
159800       MOVE 'TIHHMM-READY'        TO DCSL-BETEXT-ITEM                     
159900       MOVE DCS-TIHHMM-READY      TO DCSL-BETEXT-OLDDATA                  
160000       MOVE MID-TIHHMM-READY-IN   TO DCS-TIHHMM-READY                     
160100                                     DCSL-BETEXT-NEWDATA                  
160200       ADD +1                     TO FIL-IDSEKVNR                         
160300       PERFORM IMS-ISRT-WDR601                                            
160400     END-IF                                                               
160500                                                                          
160600     IF MID-FLINVACS-IN NOT = ALL '+'                                     
160700       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLINVACS-IN-ATTR                 
160800       MOVE 'FLINVACS'         TO DCSL-BETEXT-ITEM                        
160900       MOVE DCS-FLINVACS       TO DCSL-BETEXT-OLDDATA                     
161000       IF MID-FLINVACS-IN = YES                                           
161100          MOVE JA              TO DCS-FLINVACS                            
161200                                 DCSL-BETEXT-NEWDATA                      
161300       ELSE                                                               
161400          MOVE MID-FLINVACS-IN TO DCS-FLINVACS                            
161500                                 DCSL-BETEXT-NEWDATA                      
161600       END-IF                                                             
161700       ADD +1                  TO FIL-IDSEKVNR                            
161800       PERFORM IMS-ISRT-WDR601                                            
161900     END-IF                                                               
162000                                                                          
162100     IF MID-IDFTG-IN NOT = ALL '+'                                        
162200       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDFTG-IN-ATTR                    
162300       MOVE 'IDFTG'               TO DCSL-BETEXT-ITEM                     
162400       MOVE DCS-IDFTG             TO DCSL-BETEXT-OLDDATA                  
162500       MOVE MID-IDFTG-IN          TO DCS-IDFTG                            
162600                                     DCSL-BETEXT-NEWDATA                  
162700       ADD +1                     TO FIL-IDSEKVNR                         
162800       PERFORM IMS-ISRT-WDR601                                            
162900     END-IF                                                               
163000                                                                          
163100     IF MID-IDLEVNR-EMB-IN NOT = ALL '+'                                  
163200       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDLEVNR-EMB-IN-ATTR              
163300       MOVE 'IDLEVNR-EMB'         TO DCSL-BETEXT-ITEM                     
163400       MOVE DCS-IDLEVNR-EMB       TO DCSL-BETEXT-OLDDATA                  
163500       MOVE MID-IDLEVNR-EMB-IN    TO DCS-IDLEVNR-EMB                      
163600                                     DCSL-BETEXT-NEWDATA                  
163700       ADD +1                     TO FIL-IDSEKVNR                         
163800       PERFORM IMS-ISRT-WDR601                                            
163900     END-IF                                                               
164000                                                                          
164100     IF MID-IDPARTNR-IN NOT = ALL '+'                                     
164200       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDPARTNR-IN-ATTR                 
164300       MOVE 'IDPARTNR'            TO DCSL-BETEXT-ITEM                     
164400       MOVE DCS-IDPARTNR          TO DCSL-BETEXT-OLDDATA                  
164500       MOVE MID-IDPARTNR-IN       TO DCS-IDPARTNR                         
164600                                     DCSL-BETEXT-NEWDATA                  
164700       ADD +1                     TO FIL-IDSEKVNR                         
164800       PERFORM IMS-ISRT-WDR601                                            
164900     END-IF                                                               
165000                                                                          
165100     IF MID-FLMAINDC-IN NOT = ALL '+'                                     
165200       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLMAINDC-IN-ATTR                 
165300       MOVE 'FLMAINDC'         TO DCSL-BETEXT-ITEM                        
165400       MOVE DCS-FLMAINDC       TO DCSL-BETEXT-OLDDATA                     
165500       IF MID-FLMAINDC-IN = YES                                           
165600          MOVE JA              TO DCS-FLMAINDC                            
165700                                 DCSL-BETEXT-NEWDATA                      
165800       ELSE                                                               
165900          MOVE MID-FLMAINDC-IN TO DCS-FLMAINDC                            
166000                                 DCSL-BETEXT-NEWDATA                      
166100       END-IF                                                             
166200       ADD +1                  TO FIL-IDSEKVNR                            
166300       PERFORM IMS-ISRT-WDR601                                            
166400     END-IF                                                               
166500                                                                          
166600     IF MID-KDTRADP-IN NOT = ALL '+'                                      
166700       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDTRADP-IN-ATTR                  
166800       MOVE 'KDTRADP'             TO DCSL-BETEXT-ITEM                     
166900       MOVE DCS-KDTRADP           TO DCSL-BETEXT-OLDDATA                  
167000       MOVE MID-KDTRADP-IN        TO DCS-KDTRADP                          
167100                                     DCSL-BETEXT-NEWDATA                  
167200       ADD +1                     TO FIL-IDSEKVNR                         
167300       PERFORM IMS-ISRT-WDR601                                            
167400     END-IF                                                               
167500                                                                          
167600     IF MID-IDLEGSEL-IN NOT = ALL '+'                                     
167700       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDLEGSEL-IN-ATTR                 
167800       MOVE 'IDLEGSEL'            TO DCSL-BETEXT-ITEM                     
167900       MOVE DCS-IDLEGSEL          TO DCSL-BETEXT-OLDDATA                  
168000       MOVE MID-IDLEGSEL-IN       TO DCS-IDLEGSEL                         
168100                                     DCSL-BETEXT-NEWDATA                  
168200       ADD +1                     TO FIL-IDSEKVNR                         
168300       PERFORM IMS-ISRT-WDR601                                            
168400     END-IF                                                               
168500                                                                          
168600     IF MID-FLSTOREF-IN NOT = ALL '+'                                     
168700       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLSTOREF-IN-ATTR                 
168800       MOVE 'FLSTOREF'         TO DCSL-BETEXT-ITEM                        
168900       MOVE DCS-FLSTOREF       TO DCSL-BETEXT-OLDDATA                     
169000       IF MID-FLSTOREF-IN = YES                                           
169100          MOVE JA              TO DCS-FLSTOREF                            
169200                                  DCSL-BETEXT-NEWDATA                     
169300       ELSE                                                               
169400          MOVE MID-FLSTOREF-IN TO DCS-FLSTOREF                            
169500                                  DCSL-BETEXT-NEWDATA                     
169600       END-IF                                                             
169700       ADD +1                  TO FIL-IDSEKVNR                            
169800       PERFORM IMS-ISRT-WDR601                                            
169900     END-IF                                                               
170000                                                                          
170100     IF MID-IDSKYLT-IN NOT = ALL '+'                                      
170200       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDSKYLT-IN-ATTR                  
170300       MOVE 'IDSKYLT'             TO DCSL-BETEXT-ITEM                     
170400       MOVE DCS-IDSKYLT-DB        TO DCSL-BETEXT-OLDDATA                  
170500       MOVE MID-IDSKYLT-IN        TO DCS-IDSKYLT-DB                       
170600                                     DCSL-BETEXT-NEWDATA                  
170700       ADD +1                     TO FIL-IDSEKVNR                         
170800       PERFORM IMS-ISRT-WDR601                                            
170900     END-IF                                                               
171000                                                                          
171100     IF MID-FLSTOFC-IN NOT = ALL '+'                                      
171200       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLSTOFC-IN-ATTR                  
171300       MOVE 'FLSTOFC'          TO DCSL-BETEXT-ITEM                        
171400       MOVE DCS-FLSTOFC        TO DCSL-BETEXT-OLDDATA                     
171500       IF MID-FLSTOFC-IN = YES                                            
171600          MOVE JA              TO DCS-FLSTOFC                             
171700                                  DCSL-BETEXT-NEWDATA                     
171800       ELSE                                                               
171900          MOVE MID-FLSTOFC-IN  TO DCS-FLSTOFC                             
172000                                  DCSL-BETEXT-NEWDATA                     
172100       END-IF                                                             
172200       ADD +1                  TO FIL-IDSEKVNR                            
172300       PERFORM IMS-ISRT-WDR601                                            
172400     END-IF                                                               
172500                                                                          
172600     IF MID-FLLPO-IN NOT = ALL '+'                                        
172700       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLLPO-IN-ATTR                    
172800       MOVE 'FLLPO'            TO DCSL-BETEXT-ITEM                        
172900       MOVE DCS-FLLPO          TO DCSL-BETEXT-OLDDATA                     
173000       IF MID-FLLPO-IN = YES                                              
173100          MOVE JA              TO DCS-FLLPO                               
173200                                  DCSL-BETEXT-NEWDATA                     
173300       ELSE                                                               
173400          MOVE MID-FLLPO-IN    TO DCS-FLLPO                               
173500                                  DCSL-BETEXT-NEWDATA                     
173600       END-IF                                                             
173700       ADD +1                  TO FIL-IDSEKVNR                            
173800       PERFORM IMS-ISRT-WDR601                                            
173900     END-IF                                                               
174000                                                                          
174100     PERFORM IMS-REPL-WDB601                                              
174200                                                                          
174300     PERFORM IMS-GHNP-WDB602                                              
174400     IF SEGMENT-FINNS                                                     
174500       IF  MID-BEGMT-RAD1-INV(1:1)  = SPACE                               
174600       AND MID-ADGMT-GATA-INV(1:1)  = SPACE                               
174700         PERFORM IMS-DLET-WDB602                                          
174800         MOVE 'WDB602'          TO DCSL-BETEXT-ITEM                       
174900         MOVE 'DLET DC-SEGMENT' TO DCSL-BETEXT-OLDDATA                    
175000         MOVE DCS-IDDC          TO DCSL-BETEXT-NEWDATA                    
175100                                                                          
175200         ADD +1                 TO  FIL-IDSEKVNR                          
175300         PERFORM IMS-ISRT-WDR601                                          
175400       ELSE                                                               
175500         IF MID-BEGMT-RAD1-INV NOT = ALL '+'                              
175600           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-BEGMT-RAD1-INV-ATTR          
175700           MOVE 'BEGMT-RAD1-INV'   TO DCSL-BETEXT-ITEM                    
175800           MOVE INV-BEGMT-RAD1     TO DCSL-BETEXT-OLDDATA                 
175900           MOVE MID-BEGMT-RAD1-INV TO INV-BEGMT-RAD1                      
176000                                      DCSL-BETEXT-NEWDATA                 
176100           ADD +1                  TO FIL-IDSEKVNR                        
176200           PERFORM IMS-ISRT-WDR601                                        
176300         END-IF                                                           
176400                                                                          
176500         IF MID-BEGMT-RAD2-INV NOT = ALL '+'                              
176600           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-BEGMT-RAD2-INV-ATTR          
176700           MOVE 'BEGMT-RAD2-INV'   TO DCSL-BETEXT-ITEM                    
176800           MOVE INV-BEGMT-RAD2     TO DCSL-BETEXT-OLDDATA                 
176900           MOVE MID-BEGMT-RAD2-INV TO INV-BEGMT-RAD2                      
177000                                      DCSL-BETEXT-NEWDATA                 
177100           ADD +1                  TO FIL-IDSEKVNR                        
177200           PERFORM IMS-ISRT-WDR601                                        
177300         END-IF                                                           
177400                                                                          
177500         IF MID-ADGMT-GATA-INV NOT = ALL '+'                              
177600           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-ADGMT-GATA-INV-ATTR          
177700           MOVE 'ADGMT-GATA-INV'   TO DCSL-BETEXT-ITEM                    
177800           MOVE INV-ADGMT-GATA     TO DCSL-BETEXT-OLDDATA                 
177900           MOVE MID-ADGMT-GATA-INV TO INV-ADGMT-GATA                      
178000                                      DCSL-BETEXT-NEWDATA                 
178100           ADD +1                  TO FIL-IDSEKVNR                        
178200           PERFORM IMS-ISRT-WDR601                                        
178300         END-IF                                                           
178400                                                                          
178500         IF MID-ADPOSTNR-INV NOT = ALL '+'                                
178600         OR MID-ADCITY-INV   NOT = ALL '+'                                
178700           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-ADPOSTNR-INV-ATTR            
178800           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-ADCITY-INV-ATTR              
178900           MOVE 'ADGMT-PADR-INV'  TO DCSL-BETEXT-ITEM                     
179000           MOVE INV-ADGMT-PADR    TO DCSL-BETEXT-OLDDATA                  
179100           IF MID-ADPOSTNR-INV NOT = ALL '+'                              
179200             MOVE MID-ADPOSTNR-INV  TO INV-ADGMT-PADR(1:10)               
179300           END-IF                                                         
179400           IF MID-ADCITY-INV   NOT = ALL '+'                              
179500             MOVE MID-ADCITY-INV    TO INV-ADGMT-PADR(11:20)              
179600           END-IF                                                         
179700           MOVE INV-ADGMT-PADR    TO DCSL-BETEXT-NEWDATA                  
179800           ADD +1                 TO FIL-IDSEKVNR                         
179900           PERFORM IMS-ISRT-WDR601                                        
180000         END-IF                                                           
180100                                                                          
180200         IF MID-ADGMT-LAND-INV NOT = ALL '+'                              
180300           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-ADGMT-LAND-INV-ATTR          
180400           MOVE 'ADGMT-LAND-INV'   TO DCSL-BETEXT-ITEM                    
180500           MOVE INV-ADGMT-LAND     TO DCSL-BETEXT-OLDDATA                 
180600           MOVE MID-ADGMT-LAND-INV TO INV-ADGMT-LAND                      
180700                                      DCSL-BETEXT-NEWDATA                 
180800           ADD +1                  TO FIL-IDSEKVNR                        
180900           PERFORM IMS-ISRT-WDR601                                        
181000         END-IF                                                           
181100                                                                          
181200         PERFORM IMS-REPL-WDB602                                          
181300       END-IF                                                             
181400     ELSE                                                                 
181500       IF MID-BEGMT-RAD1-INV NOT = ALL '+'                                
181600         MOVE MID-BEGMT-RAD1-INV   TO INV-BEGMT-RAD1                      
181700       END-IF                                                             
181800       IF MID-BEGMT-RAD2-INV NOT = ALL '+'                                
181900         MOVE MID-BEGMT-RAD2-INV   TO INV-BEGMT-RAD2                      
182000       END-IF                                                             
182100       IF MID-ADGMT-GATA-INV NOT = ALL '+'                                
182200         MOVE MID-ADGMT-GATA-INV   TO INV-ADGMT-GATA                      
182300       END-IF                                                             
182400       IF MID-ADPOSTNR-INV NOT = ALL '+'                                  
182500         MOVE MID-ADPOSTNR-INV     TO INV-ADGMT-PADR(1:10)                
182600       END-IF                                                             
182700       IF MID-ADCITY-INV NOT = ALL '+'                                    
182800         MOVE MID-ADCITY-INV       TO INV-ADGMT-PADR(11:20)               
182900       END-IF                                                             
183000       IF MID-ADGMT-LAND-INV NOT = ALL '+'                                
183100         MOVE MID-ADGMT-LAND-INV   TO INV-ADGMT-LAND                      
183200       END-IF                                                             
183300       PERFORM IMS-ISRT-WDB602                                            
183400                                                                          
183500       MOVE 'WDB602'          TO DCSL-BETEXT-ITEM                         
183600       MOVE 'NYTT DC-SEGMENT' TO DCSL-BETEXT-OLDDATA                      
183700       MOVE DCS-IDDC          TO DCSL-BETEXT-NEWDATA                      
183800                                                                          
183900       ADD +1                 TO  FIL-IDSEKVNR                            
184000       PERFORM IMS-ISRT-WDR601                                            
184100     END-IF                                                               
184200     .                                                                    
184300     EJECT                                                                
184400 MFS-RENSA-FAELT-UT SECTION.                                              
184500                                                                          
184600*    --- ALLA UTDATA-FÄLT                                                 
184700     MOVE MFS-RENSA-FAELT TO MOD-FLNYSEG                                  
184800                             MOD-BEGMT-RAD1                               
184900                             MOD-BEGMT-RAD2                               
185000                             MOD-IDLANDX2                                 
185100                             MOD-IDLEVNR                                  
185200                             MOD-KDDC                                     
185300                             MOD-FLWEBDC                                  
185400                             MOD-FLINVACS                                 
185500                             MOD-IDTIDZON                                 
185600                             MOD-IDFTG                                    
185700                             MOD-IDLEVNR-EMB                              
185800                             MOD-IDPARTNR                                 
185900                             MOD-IDLEGSEL                                 
186000                             MOD-KDTRADP                                  
186100                             MOD-KDVALISO                                 
186200                             MOD-IDVAT                                    
186300                             MOD-ADGMT-GATA                               
186400                             MOD-ADPOSTNR                                 
186500                             MOD-ADCITY                                   
186600                             MOD-ADGMT-LAND                               
186700                             MOD-TIHHMM-START                             
186800                             MOD-TIHHMM-READY                             
186900                             MOD-FLMAINDC                                 
187000                             MOD-FLSTOREF                                 
187100                             MOD-FLSTOFC                                  
187200                             MOD-BEGMT-RAD1-INV                           
187300                             MOD-BEGMT-RAD2-INV                           
187400                             MOD-ADGMT-GATA-INV                           
187500                             MOD-ADPOSTNR-INV                             
187600                             MOD-ADCITY-INV                               
187700                             MOD-ADGMT-LAND-INV                           
187800                             MOD-FLLPO                                    
187900     .                                                                    
188000     EJECT                                                                
188100 MFS-RENSA-FAELT-IN SECTION.                                              
188200                                                                          
188300*    --- ALLA INDATA-FÄLT                                                 
188400     MOVE MFS-RENSA-FAELT TO MOD-FLNYSEG                                  
188500                             MOD-BEGMT-RAD1                               
188600                             MOD-BEGMT-RAD2                               
188700                             MOD-IDLANDX2-IN                              
188800                             MOD-IDLEVNR-IN                               
188900                             MOD-KDDC-IN                                  
189000                             MOD-FLWEBDC-IN                               
189100                             MOD-FLINVACS-IN                              
189200                             MOD-IDTIDZON-IN                              
189300                             MOD-IDFTG-IN                                 
189400                             MOD-IDLEVNR-EMB-IN                           
189500                             MOD-IDPARTNR-IN                              
189600                             MOD-IDLEGSEL-IN                              
189700                             MOD-KDTRADP-IN                               
189800                             MOD-KDVALISO-IN                              
189900                             MOD-IDVAT                                    
190000                             MOD-ADGMT-GATA                               
190100                             MOD-ADPOSTNR                                 
190200                             MOD-ADCITY                                   
190300                             MOD-ADGMT-LAND                               
190400                             MOD-TIHHMM-START-IN                          
190500                             MOD-TIHHMM-READY-IN                          
190600                             MOD-FLMAINDC-IN                              
190700                             MOD-FLSTOREF-IN                              
190800                             MOD-FLSTOFC-IN                               
190900                             MOD-BEGMT-RAD1-INV                           
191000                             MOD-BEGMT-RAD2-INV                           
191100                             MOD-ADGMT-GATA-INV                           
191200                             MOD-ADPOSTNR-INV                             
191300                             MOD-ADCITY-INV                               
191400                             MOD-ADGMT-LAND-INV                           
191500                             MOD-FLLPO-IN                                 
191600                             MOD-IDSKYLT-IN                               
191700     .                                                                    
191800     EJECT                                                                
191900 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
192000                                                                          
192100*    --- ALLA UTDATA-FÄLT                                                 
192200     MOVE MFS-ROER-EJ-FAELT TO MOD-FLNYSEG                                
192300                               MOD-BEGMT-RAD1                             
192400                               MOD-BEGMT-RAD2                             
192500                               MOD-IDLANDX2                               
192600                               MOD-IDLEVNR                                
192700                               MOD-KDDC                                   
192800                               MOD-FLWEBDC                                
192900                               MOD-FLINVACS                               
193000                               MOD-IDTIDZON                               
193100                               MOD-IDFTG                                  
193200                               MOD-IDLEVNR-EMB                            
193300                               MOD-IDPARTNR                               
193400                               MOD-IDLEGSEL                               
193500                               MOD-KDTRADP                                
193600                               MOD-KDVALISO                               
193700                               MOD-IDVAT                                  
193800                               MOD-ADGMT-GATA                             
193900                               MOD-ADPOSTNR                               
194000                               MOD-ADCITY                                 
194100                               MOD-ADGMT-LAND                             
194200                               MOD-TIHHMM-START                           
194300                               MOD-TIHHMM-READY                           
194400                               MOD-FLMAINDC                               
194500                               MOD-FLSTOREF                               
194600                               MOD-FLSTOFC                                
194700                               MOD-BEGMT-RAD1-INV                         
194800                               MOD-BEGMT-RAD2-INV                         
194900                               MOD-ADGMT-GATA-INV                         
195000                               MOD-ADPOSTNR-INV                           
195100                               MOD-ADCITY-INV                             
195200                               MOD-ADGMT-LAND-INV                         
195300                               MOD-FLLPO                                  
195400                               MOD-IDSKYLT-DB                             
195500     .                                                                    
195600     EJECT                                                                
195700 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
195800                                                                          
195900*    --- ALLA INDATA-FÄLT                                                 
196000     MOVE MFS-ROER-EJ-FAELT TO MOD-FLNYSEG                                
196100                               MOD-IDLANDX2-IN                            
196200                               MOD-IDLEVNR-IN                             
196300                               MOD-KDDC-IN                                
196400                               MOD-FLWEBDC-IN                             
196500                               MOD-FLINVACS-IN                            
196600                               MOD-IDTIDZON-IN                            
196700                               MOD-IDFTG-IN                               
196800                               MOD-IDLEVNR-EMB-IN                         
196900                               MOD-IDPARTNR-IN                            
197000                               MOD-IDLEGSEL-IN                            
197100                               MOD-KDTRADP-IN                             
197200                               MOD-KDVALISO-IN                            
197300*                              MOD-IDVAT                                  
197400                               MOD-TIHHMM-START-IN                        
197500                               MOD-TIHHMM-READY-IN                        
197600                               MOD-FLMAINDC-IN                            
197700                               MOD-FLSTOREF-IN                            
197800                               MOD-FLSTOFC-IN                             
197900                               MOD-FLLPO-IN                               
198000                               MOD-IDSKYLT-IN                             
198100     .                                                                    
198200     EJECT                                                                
198300 MFS-FORM-ATTR SECTION.                                                   
198400                                                                          
198500*    --- ALLA INDATA-FÄLT                                                 
198600     MOVE MFS-FORMATETS-ATTR TO MOD-FLNYSEG-ATTR                          
198700                                MOD-BEGMT-RAD1-ATTR                       
198800                                MOD-BEGMT-RAD2-ATTR                       
198900                                MOD-IDLEVNR-IN-ATTR                       
199000                                MOD-KDDC-IN-ATTR                          
199100                                MOD-FLWEBDC-IN-ATTR                       
199200                                MOD-FLINVACS-IN-ATTR                      
199300                                MOD-IDTIDZON-IN-ATTR                      
199400                                MOD-IDFTG-IN-ATTR                         
199500                                MOD-IDLEVNR-EMB-IN-ATTR                   
199600                                MOD-IDPARTNR-IN-ATTR                      
199700                                MOD-KDTRADP-IN-ATTR                       
199800                                MOD-IDLEGSEL-IN-ATTR                      
199900                                MOD-KDVALISO-IN-ATTR                      
200000                                MOD-IDVAT-ATTR                            
200100                                MOD-ADGMT-GATA-ATTR                       
200200                                MOD-ADPOSTNR-ATTR                         
200300                                MOD-ADCITY-ATTR                           
200400                                MOD-ADGMT-LAND-ATTR                       
200500                                MOD-TIHHMM-START-IN-ATTR                  
200600                                MOD-TIHHMM-READY-IN-ATTR                  
200700                                MOD-FLMAINDC-IN-ATTR                      
200800                                MOD-FLSTOREF-IN-ATTR                      
200900                                MOD-FLSTOFC-IN-ATTR                       
201000                                MOD-BEGMT-RAD1-INV-ATTR                   
201100                                MOD-BEGMT-RAD2-INV-ATTR                   
201200                                MOD-ADGMT-GATA-INV-ATTR                   
201300                                MOD-ADPOSTNR-INV-ATTR                     
201400                                MOD-ADCITY-INV-ATTR                       
201500                                MOD-ADGMT-LAND-INV-ATTR                   
201600                                MOD-FLLPO-IN-ATTR                         
201700                                MOD-IDSKYLT-IN-ATTR                       
201800     .                                                                    
201900     EJECT                                                                
202000* --- IMS SEKTIONER ---                                                   
202100     SKIP3                                                                
202200 IMS-GET-MSG SECTION.                                                     
202300                                                                          
202400     MOVE '  QC' TO GODK-STATUSKODER                                      
202500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
202600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
202700     PERFORM IMS-STATUSKONTROLL                                           
202800     .                                                                    
202900     SKIP3                                                                
203000 IMS-INSERT-MSG SECTION.                                                  
203100                                                                          
203200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
203300     MOVE SPACE TO GODK-STATUSKODER                                       
203400     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
203500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
203600     PERFORM IMS-STATUSKONTROLL                                           
203700     .                                                                    
203800     EJECT                                                                
203900 IMS-GHU-WDB601 SECTION.                                                  
204000                                                                          
204100     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
204200          DELIMITED BY SIZE INTO SSA1                                     
204300     MOVE '  GE' TO GODK-STATUSKODER                                      
204400     CALL CBLTDLI USING GHU WDB6-PCB DLI-IO-WDB601 SSA1                   
204500     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
204600     PERFORM IMS-STATUSKONTROLL                                           
204700     .                                                                    
204800     SKIP3                                                                
204900 IMS-GU-WDB601-MAINDC SECTION.                                            
205000                                                                          
205100     STRING 'WDB601  (IDFTG    =' DCS-IDFTG                               
205200                    '&FLMAINDC =' JA ')'                                  
205300          DELIMITED BY SIZE INTO SSA1                                     
205400     MOVE '  GE' TO GODK-STATUSKODER                                      
205500     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
205600     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
205700     PERFORM IMS-STATUSKONTROLL                                           
205800     .                                                                    
205900     EJECT                                                                
206000 IMS-ISRT-WDB601 SECTION.                                                 
206100                                                                          
206200     MOVE 'WDB601 ' TO SSA1                                               
206300     MOVE '    ' TO GODK-STATUSKODER                                      
206400     CALL CBLTDLI USING ISRT WDB6-PCB DLI-IO-WDB601 SSA1                  
206500     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
206600     PERFORM IMS-STATUSKONTROLL                                           
206700     .                                                                    
206800     SKIP3                                                                
206900 IMS-REPL-WDB601 SECTION.                                                 
207000                                                                          
207100     MOVE '  ' TO GODK-STATUSKODER                                        
207200     CALL CBLTDLI USING REPL WDB6-PCB DLI-IO-WDB601                       
207300     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
207400     PERFORM IMS-STATUSKONTROLL                                           
207500     .                                                                    
207600     EJECT                                                                
207700 IMS-GHNP-WDB602 SECTION.                                                 
207800                                                                          
207900     MOVE 'WDB602  '          TO SSA1                                     
208000     MOVE '  GE' TO GODK-STATUSKODER                                      
208100     CALL CBLTDLI USING GHNP WDB6-PCB DLI-IO-WDB602 SSA1                  
208200     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
208300     PERFORM IMS-STATUSKONTROLL                                           
208400     .                                                                    
208500     SKIP3                                                                
208600 IMS-GNP-WDB602 SECTION.                                                  
208700                                                                          
208800     MOVE 'WDB602  '          TO SSA1                                     
208900     MOVE '  GE' TO GODK-STATUSKODER                                      
209000     CALL CBLTDLI USING GNP WDB6-PCB DLI-IO-WDB602 SSA1                   
209100     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
209200     PERFORM IMS-STATUSKONTROLL                                           
209300     .                                                                    
209400     SKIP3                                                                
209500 IMS-ISRT-WDB602 SECTION.                                                 
209600                                                                          
209700     MOVE 'WDB602 ' TO SSA1                                               
209800     MOVE '    ' TO GODK-STATUSKODER                                      
209900     CALL CBLTDLI USING ISRT WDB6-PCB DLI-IO-WDB602 SSA1                  
210000     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
210100     PERFORM IMS-STATUSKONTROLL                                           
210200     .                                                                    
210300     SKIP3                                                                
210400 IMS-REPL-WDB602 SECTION.                                                 
210500                                                                          
210600     MOVE '  ' TO GODK-STATUSKODER                                        
210700     CALL CBLTDLI USING REPL WDB6-PCB DLI-IO-WDB602                       
210800     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
210900     PERFORM IMS-STATUSKONTROLL                                           
211000     .                                                                    
211100     EJECT                                                                
211200 IMS-ISRT-WDR601 SECTION.                                                 
211300                                                                          
211400     MOVE 'WDR601 ' TO SSA1                                               
211500     MOVE '    ' TO GODK-STATUSKODER                                      
211600     CALL CBLTDLI USING ISRT WDR6-PCB DLI-IO-WDR601 SSA1                  
211700     MOVE WDR6-STATUS-CODE TO STATUS-WS                                   
211800     PERFORM IMS-STATUSKONTROLL                                           
211900     .                                                                    
212000     SKIP3                                                                
212100                                                                          
212200 IMS-DLET-WDB602 SECTION.                                                 
212300                                                                          
212400     MOVE '  ' TO GODK-STATUSKODER                                        
212500     CALL CBLTDLI USING DLET WDB6-PCB DLI-IO-WDB602                       
212600     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
212700     PERFORM IMS-STATUSKONTROLL                                           
212800     .                                                                    
212900                                                                          
213000 IMS-STATUSKONTROLL SECTION.                                              
213100                                                                          
213200     SET STATUS-IX TO 1                                                   
213300     SEARCH GODK-STATUS                                                   
213400       AT END                                                             
213500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
213600         DELIMITED BY SIZE INTO FELTEXT                                   
213700         CALL FELLOG                                                      
213800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
213900         CONTINUE                                                         
214000     END-SEARCH                                                           
214100     .                                                                    
