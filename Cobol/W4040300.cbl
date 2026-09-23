000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4040300.                                                
000300 AUTHOR.         OLGRENER LASSI.                                          
000400 DATE-WRITTEN.   04/11/01.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        HANTERING AV DC-STYRREGISTER WDB6                                
000900*                                                                         
001000*        PROGRAMMET UPPDATERAR WDB6, WDR6                                 
001100*                                                                         
001200*    INDATA.                                                              
001300*        TRANSAKTION: W4T403                                              
001400*        MID:         W4I40301                                            
001500*                                                                         
001600*    UTDATA.                                                              
001700*        MOD:         W4O40301                                            
001800*    CHANGE LOG:                                                          
001900*      DD/MM/YY                                                           
002000*      28/01/14 - DATTA ARUP      - ENABLE TO START AREA 88               
002100*                                   ORDERS UPTO 5 DAYS.                   
002200*                                   SCR 10200572                          
002300*      10/02/14 - REDDY RAHUL     - START RTN W271B9 TO UPDATE            
002400*                                   IDDC-REF ON WDK711 FOR ALL            
002500*                                   APPLICABLE PARTS.                     
002600*                                   ALLOW ONLY CN DC'S TO GET             
002700*                                   REFILL FROM CN NDC'S.                 
002800*                                   SCR 10205394                          
002900                                                                          
003000     SKIP3                                                                
003100 ENVIRONMENT DIVISION.                                                    
003200                                                                          
003300 DATA DIVISION.                                                           
003400     EJECT                                                                
003500 WORKING-STORAGE SECTION.                                                 
003600 77  IDPGM                       PIC X(08)   VALUE 'W4040300'.            
003700 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003800 77  WS-REWILSON                 PIC 9.9(2).                              
003900 77  WS-REWILSON-P               PIC S9V9(2) VALUE ZERO COMP-3.           
004000 77  WS-PRARTSTD                 PIC 9(3).                                
004100                                                                          
004200 77  JA                          PIC X       VALUE 'J'.                   
004300 77  YES                         PIC X       VALUE 'Y'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004500 77  NOO                         PIC X       VALUE 'N'.                   
004600                                                                          
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
005800     88  EGEN-MID                            VALUE '4403'.                
005900     88  GODK-MID                            VALUE '4402' '0551'.         
006000     88  HELP-MID                            VALUE '0551'.                
006100     EJECT                                                                
006200*    --- VALID DC CODES                                                   
006300*01  -COPY WWDC99                                                         
006400*    --- VALID DC CODES                                                   
006500*01  -COPY WWDC99 -PRE REF-                                               
006501                                                                          
006510*01  -COPY WWDCKONS.                                                      
006600     EJECT                                                                
006700*    --- AREA FOR SOP                                                     
006800 01  W-PROG-TO-PROG-SW.                                                   
006900*    03 -COPY WMSGSOP                                                     
007000  SKIP3                                                                   
007100*    --- DATA SOM SKICKAS TILL SOP                                        
007200 01  PARM-TESYMBV.                                                        
007300     03  FILLER                  PIC X(3)    VALUE 'DC('.                 
007400     03  PARM-IDDC               PIC X(2).                                
007500     03  FILLER                  PIC X(1)    VALUE ')'.                   
007600     03  FILLER                  PIC X(9)    VALUE 'DCREFOLD('.           
007700     03  PARM-IDDC-REF-OLD       PIC X(2).                                
007800     03  FILLER                  PIC X(1)    VALUE ')'.                   
007900     03  FILLER                  PIC X(9)    VALUE 'DCREFNEW('.           
008000     03  PARM-IDDC-REF-NEW       PIC X(2).                                
008100     03  FILLER                  PIC X(1)    VALUE ')'.                   
008200     EJECT                                                                
008300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
008400 01  GENERELLA-SUBPROGRAM.                                                
008500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
008600     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
008700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008900     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
009000     EJECT                                                                
009100                                                                          
009200*   -COPY WDECAREA.                                                       
009300                                                                          
009400     EJECT                                                                
009500*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
009600*01 -COPY WMEDAREA                                                        
009700     SKIP3                                                                
009800 01  MESSAGE-CODES.                                                       
009900     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
010000     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
010100     03  ERR-UPD-NOT-ALLOWED     PIC X(3)    VALUE '007'.                 
010200     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
010300     03  ERR-DC-MISSING          PIC X(3)    VALUE '026'.                 
010400     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
010500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
010600     EJECT                                                                
010700*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
010800*                                                                         
010900 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
011000     SKIP3                                                                
011100*01 -COPY WMSGINIT                                                        
011200     EJECT                                                                
011300*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
011400*                                                                         
011500 01  SPAR-AREA.                                                           
011600     03  SPAR-IDTRANS           PIC X(4)    VALUE '4403'.                 
011700     EJECT                                                                
011800*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
011900*                                                                         
012000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
012100     SKIP3                                                                
012200*01  MID -COPY W4I40301                                                   
012300     EJECT                                                                
012400 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
012500     SKIP3                                                                
012600*01  -COPY WMSGAREA                                                       
012700     EJECT                                                                
012800     03  MOD REDEFINES MSG-AREA.                                          
012900*      05  -COPY W4O40301                                                 
013000     EJECT                                                                
013100 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
013200     SKIP3                                                                
013300*01  -COPY WMFSAREA                                                       
013400     EJECT                                                                
013500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
013600*                                                                         
013700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013800     SKIP3                                                                
013900 01  NYCKLAR-TILL-DLI.                                                    
014000     03  W-IDDC-X.                                                        
014100         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
014101     03  W-IDDC-REF-X.                                                    
014102         05  W-IDDC-REF          PIC X(2)    VALUE SPACE.                 
014110     03  W-WDB615KY-X.                                                    
014120         05  W-IDTRANS-B6        PIC X(4)    VALUE SPACE.                 
014130         05  W-IDDC-REF-B6       PIC X(2)    VALUE SPACE.                 
014200     SKIP2                                                                
014300*    --- STATUS-KOD FRÅN IMS                                              
014400 01  STATUS-WS                   PIC XX.                                  
014500     88  SEGMENT-FINNS                       VALUE '  '.                  
014600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
014700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
014800     SKIP2                                                                
014900 01  GODK-STATUSKODER.                                                    
015000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015100     SKIP3                                                                
015200 01  SSA1                        PIC X(64).                               
015300 01  SSA2                        PIC X(64).                               
015400     EJECT                                                                
015500*    --- IMS FUNKTIONSKODER                                               
015600*01  -COPY W0003                                                          
015700     EJECT                                                                
015800*    ---  DLI INPUT-OUTPUT AREA                                           
015900                                                                          
016000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
016100 01  DLI-IO-WDB601.                                                       
016200*    03  -COPY WDB601                                                     
016210                                                                          
016220 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB615'.                      
016230 01  DLI-IO-WDB615.                                                       
016240*    03  -COPY WDB615                                                     
016250                                                                          
016260 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB616'.                      
016270 01  DLI-IO-WDB616.                                                       
016280*    03  -COPY WDB616                                                     
016300     EJECT                                                                
016400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR601'.                      
016500 01  DLI-IO-WDR601.                                                       
016600*    03  -COPY WDR601                                                     
016700*    05  LOGG  -COPY W414DCSA          -RED FIL-WDR601-DATA.              
016800     EJECT                                                                
016900 LINKAGE SECTION.                                                         
017000*01  -COPY W0009   -PRE MSG-                                              
017100*01  -COPY W0009   -PRE ALT-                                              
017200*01  -COPY W0008   -PRE WDP7-                                             
017300     05  FILLER                  PIC X.                                   
017400                                                                          
017500*01  -COPY W0008  -PRE WDB6-                                              
017600     05  FILLER                  PIC X.                                   
017700     EJECT                                                                
017800*01  -COPY W0008  -PRE WDR6-                                              
017900     05  FILLER                  PIC X.                                   
018000     EJECT                                                                
018100 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB                                
018200                           WDP7-PCB WDB6-PCB WDR6-PCB.                    
018300 MAIN SECTION.                                                            
018400     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB                                
018500                           WDP7-PCB WDB6-PCB WDR6-PCB.                    
018600                                                                          
018700     PERFORM IMS-GET-MSG                                                  
018800     IF SEGMENT-FINNS                                                     
018900       PERFORM A-INIT                                                     
019000       PERFORM B-KOLLA-NYCKLAR                                            
019100       IF NYCKLAR-OK                                                      
019200         IF MFS-UPDATE                                                    
019300           PERFORM G-KOLLA-INPUT                                          
019400           IF INDATA-OK                                                   
019500             PERFORM H-UPPDATERA                                          
019600           END-IF                                                         
019700         ELSE                                                             
019800           IF MFS-FIRST                                                   
019900             PERFORM C-FOERSTA-SIDA                                       
020000           ELSE                                                           
020100             PERFORM E-SAMMA-SIDA                                         
020200           END-IF                                                         
020300         END-IF                                                           
020400         PERFORM F-LAES-VISA-INFO                                         
020500       END-IF                                                             
020600       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O40301 + 4                      
020700       PERFORM IMS-INSERT-MSG                                             
020800     END-IF                                                               
020900                                                                          
021000     MOVE ZERO TO RETURN-CODE                                             
021100     GOBACK                                                               
021200     .                                                                    
021300     EJECT                                                                
021400 A-INIT SECTION.                                                          
021500                                                                          
021600     IF MSG-DUBBLA-TRANSKODER                                             
021700       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I40301                 
021800       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
021900       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
022000     ELSE                                                                 
022100       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I40301                  
022200       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
022300       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
022400     END-IF                                                               
022500                                                                          
022600     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
022700     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
022800     MOVE MFS-IDTRANS TO W-IDTRANS                                        
022900                                                                          
023000     MOVE LOW-VALUE TO MSG-AREA                                           
023100     MOVE 'W4O403N1' TO MFS-IDMOD                                         
023200     MOVE '4403' TO MOD-IDTRANS                                           
023300     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
023400                                                                          
023500     IF EGEN-MID OR HELP-MID                                              
023600       CONTINUE                                                           
023700     ELSE                                                                 
023800       MOVE SPACE TO MFS-KDTRTYP                                          
023900       MOVE '7' TO MFS-IDPFK                                              
024000     END-IF                                                               
024100     MOVE IDPGM                TO  FIL-IDPGM                              
024200     ACCEPT FIL-TIREGDAT       FROM  DATE                                 
024300     ACCEPT FIL-TIKLOCK        FROM  TIME                                 
024400     MOVE ZERO                 TO  FIL-IDSEKVNR                           
024500     MOVE 'W414'               TO  FIL-CT-IDSYSTEM                        
024600     MOVE 'DCS'                TO  FIL-CT-IDPTYP                          
024700     MOVE 'A'                  TO  FIL-CT-IDVTYP                          
024710                                                                          
024720     MOVE SPACE                TO MED-IDMFSFEL                            
024800     .                                                                    
024900     EJECT                                                                
025000 B-KOLLA-NYCKLAR SECTION.                                                 
025100                                                                          
025200     MOVE ALL '+'           TO MSGI-WMSGINIT                              
025300     MOVE '001'             TO MSGI-KDCALL                                
025400     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
025500     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
025600     MOVE '4403'            TO MSGI-IDTRANS                               
025700*??  IF GODK-MID                                                          
025800         MOVE MID-IDDC-IN     TO MSGI-IDDC-KEY                            
025900*    END-IF                                                               
026000     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
026100     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
026200                                                                          
026300*    - SPRÅK SOM SKA ANVÄNDAS AV WMEDKONV                                 
026400     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
026500                                                                          
026600     MOVE JA TO NYCKLAR-SW                                                
026700                                                                          
026800                                                                          
026900*    -- KONTROLL AV IDDC                                                  
027000     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
027100                                                                          
027200     IF MID-IDDC-IN NOT = ALL '+'                                         
027300       MOVE '7'         TO MFS-IDPFK                                      
027400       MOVE SPACE       TO MFS-KDTRTYP                                    
027500     END-IF                                                               
027600     MOVE MSGI-IDDC-KEY TO W-IDDC                                         
027700                           MOD-IDDC-UT                                    
027800                           DCSL-IDDC                                      
027900                                                                          
028000     MOVE MSGI-IDUSER   TO DCSL-IDUSER                                    
028100     MOVE MSGI-BEANST   TO DCSL-BEANST                                    
028200     MOVE 'OLD VALUE: ' TO DCSL-BETEXT-OLD                                
028300     MOVE 'NEW VALUE: ' TO DCSL-BETEXT-NEW                                
028400     .                                                                    
028500     EJECT                                                                
028600 C-FOERSTA-SIDA SECTION.                                                  
028700                                                                          
028800     PERFORM MFS-RENSA-FAELT-IN                                           
028900     .                                                                    
029000     EJECT                                                                
029100 E-SAMMA-SIDA SECTION.                                                    
029200                                                                          
029300     IF EGEN-MID OR HELP-MID                                              
029400       IF MID-INPUT = ALL '+'                                             
029500         PERFORM MFS-RENSA-FAELT-IN                                       
029600       ELSE                                                               
029700         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
029800         CALL WMEDKONV USING MED-WMEDAREA                                 
029900         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
030000         PERFORM EA-MID-INDATA-TILL-MOD                                   
030100       END-IF                                                             
030200     ELSE                                                                 
030300       PERFORM MFS-RENSA-FAELT-IN                                         
030400     END-IF                                                               
030500     .                                                                    
030600     EJECT                                                                
030700 EA-MID-INDATA-TILL-MOD SECTION.                                          
030800                                                                          
030900     IF MID-FLDCRET-IN = ALL '+'                                          
031000       MOVE MFS-RENSA-FAELT       TO MOD-FLDCRET-IN                       
031100     ELSE                                                                 
031200       IF MID-FLDCRET-IN = JA                                             
031300          MOVE YES                TO MOD-FLDCRET-IN                       
031400       ELSE                                                               
031500          MOVE MID-FLDCRET-IN     TO MOD-FLDCRET-IN                       
031600       END-IF                                                             
031700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLDCRET-IN-ATTR                  
031800     END-IF                                                               
031900                                                                          
032000     IF MID-FLINLHIST-IN = ALL '+'                                        
032100       MOVE MFS-RENSA-FAELT       TO MOD-FLINLHIST-IN                     
032200     ELSE                                                                 
032300       IF MID-FLINLHIST-IN = JA                                           
032400          MOVE YES                TO MOD-FLINLHIST-IN                     
032500       ELSE                                                               
032600          MOVE MID-FLINLHIST-IN   TO MOD-FLINLHIST-IN                     
032700       END-IF                                                             
032800       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLINLHIST-IN-ATTR                
032900     END-IF                                                               
033000                                                                          
033100     IF MID-FLFSEDEL-IN = ALL '+'                                         
033200       MOVE MFS-RENSA-FAELT       TO MOD-FLFSEDEL-IN                      
033300     ELSE                                                                 
033400       IF MID-FLFSEDEL-IN = JA                                            
033500          MOVE YES                TO MOD-FLFSEDEL-IN                      
033600       ELSE                                                               
033700          MOVE MID-FLFSEDEL-IN    TO MOD-FLFSEDEL-IN                      
033800       END-IF                                                             
033900       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLFSEDEL-IN-ATTR                 
034000     END-IF                                                               
034100                                                                          
034200     IF MID-FLINLREP-IN = ALL '+'                                         
034300       MOVE MFS-RENSA-FAELT       TO MOD-FLINLREP-IN                      
034400     ELSE                                                                 
034500       IF MID-FLINLREP-IN = JA                                            
034600          MOVE YES                TO MOD-FLINLREP-IN                      
034700       ELSE                                                               
034800          MOVE MID-FLINLREP-IN    TO MOD-FLINLREP-IN                      
034900       END-IF                                                             
035000       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLINLREP-IN-ATTR                 
035100     END-IF                                                               
035200                                                                          
035300     IF MID-FLPRISSPR-IN = ALL '+'                                        
035400       MOVE MFS-RENSA-FAELT       TO MOD-FLPRISSPR-IN                     
035500     ELSE                                                                 
035600       IF MID-FLPRISSPR-IN = JA                                           
035700          MOVE YES                TO MOD-FLPRISSPR-IN                     
035800       ELSE                                                               
035900          MOVE MID-FLPRISSPR-IN   TO MOD-FLPRISSPR-IN                     
036000       END-IF                                                             
036100       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLPRISSPR-IN-ATTR                
036200     END-IF                                                               
036300                                                                          
036400     IF MID-FLBINNUT-IN = ALL '+'                                         
036500       MOVE MFS-RENSA-FAELT       TO MOD-FLBINNUT-IN                      
036600     ELSE                                                                 
036700       IF MID-FLBINNUT-IN = JA                                            
036800          MOVE YES                TO MOD-FLBINNUT-IN                      
036900       ELSE                                                               
037000          MOVE MID-FLBINNUT-IN    TO MOD-FLBINNUT-IN                      
037100       END-IF                                                             
037200       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLBINNUT-IN-ATTR                 
037300     END-IF                                                               
037400                                                                          
037500     IF MID-FLSAMPAK-IN = ALL '+'                                         
037600       MOVE MFS-RENSA-FAELT       TO MOD-FLSAMPAK-IN                      
037700     ELSE                                                                 
037800       IF MID-FLSAMPAK-IN = JA                                            
037900         MOVE YES                 TO MOD-FLSAMPAK-IN                      
038000       ELSE                                                               
038100         MOVE MID-FLSAMPAK-IN     TO MOD-FLSAMPAK-IN                      
038200       END-IF                                                             
038300       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLSAMPAK-IN-ATTR                 
038400     END-IF                                                               
038500                                                                          
038600     IF MID-FLSEASBER-IN = ALL '+'                                        
038700       MOVE MFS-RENSA-FAELT       TO MOD-FLSEASBER-IN                     
038800     ELSE                                                                 
038900       IF MID-FLSEASBER-IN = JA                                           
039000          MOVE YES                TO MOD-FLSEASBER-IN                     
039100       ELSE                                                               
039200          MOVE MID-FLSEASBER-IN   TO MOD-FLSEASBER-IN                     
039300       END-IF                                                             
039400       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLSEASBER-IN-ATTR                
039500     END-IF                                                               
039600                                                                          
039700     IF MID-FLTYP6JU-IN = ALL '+'                                         
039800       MOVE MFS-RENSA-FAELT       TO MOD-FLTYP6JU-IN                      
039900     ELSE                                                                 
040000       IF MID-FLTYP6JU-IN = JA                                            
040100          MOVE YES                TO MOD-FLTYP6JU-IN                      
040200       ELSE                                                               
040300          MOVE MID-FLTYP6JU-IN    TO MOD-FLTYP6JU-IN                      
040400       END-IF                                                             
040500       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLTYP6JU-IN-ATTR                 
040600     END-IF                                                               
040700                                                                          
040800     IF MID-FLRSI-IN = ALL '+'                                            
040900       MOVE MFS-RENSA-FAELT          TO MOD-FLRSI-IN                      
041000     ELSE                                                                 
041100       IF MID-FLRSI-IN = JA                                               
041200          MOVE YES                   TO MOD-FLRSI-IN                      
041300       ELSE                                                               
041400          MOVE MID-FLRSI-IN             TO MOD-FLRSI-IN                   
041500       END-IF                                                             
041600       MOVE MFS-ADD-LAES-IN-FAELT    TO MOD-FLRSI-IN-ATTR                 
041700     END-IF                                                               
041800                                                                          
041900     IF MID-KDPORDL-IN = ALL '+'                                          
042000       MOVE MFS-RENSA-FAELT       TO MOD-KDPORDL-IN                       
042100     ELSE                                                                 
042200       IF MID-KDPORDL-IN = JA                                             
042300         MOVE YES                 TO MOD-KDPORDL-IN                       
042400       ELSE                                                               
042500         MOVE MID-KDPORDL-IN      TO MOD-KDPORDL-IN                       
042600       END-IF                                                             
042700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDPORDL-IN-ATTR                  
042800     END-IF                                                               
042900                                                                          
043000     IF MID-IDLISTNR-IN = ALL '+'                                         
043100       MOVE MFS-RENSA-FAELT       TO MOD-IDLISTNR-IN                      
043200     ELSE                                                                 
043300       MOVE MID-IDLISTNR-IN       TO MOD-IDLISTNR-IN                      
043400       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLISTNR-IN-ATTR                 
043500     END-IF                                                               
043600                                                                          
043700     IF MID-FLEXCP1-PRIO-IN = ALL '+'                                     
043800       MOVE MFS-RENSA-FAELT        TO MOD-FLEXCP1-PRIO-IN                 
043900     ELSE                                                                 
044000       IF MID-FLEXCP1-PRIO-IN = JA                                        
044100          MOVE YES                 TO MOD-FLEXCP1-PRIO-IN                 
044200       ELSE                                                               
044300          MOVE MID-FLEXCP1-PRIO-IN TO MOD-FLEXCP1-PRIO-IN                 
044400       END-IF                                                             
044500       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-FLEXCP1-PRIO-IN-ATTR            
044600     END-IF                                                               
044700                                                                          
044800                                                                          
044900     IF MID-FLEXCP1-REFBEO-IN = ALL '+'                                   
045000       MOVE MFS-RENSA-FAELT          TO MOD-FLEXCP1-REFBEO-IN             
045100     ELSE                                                                 
045500       MOVE MID-FLEXCP1-REFBEO-IN    TO MOD-FLEXCP1-REFBEO-IN             
045700       MOVE MFS-ADD-LAES-IN-FAELT    TO MOD-FLEXCP1-REFBEO-IN-ATTR        
045800     END-IF                                                               
045900                                                                          
046000     IF MID-FLEXCP2-REFBEO-IN = ALL '+'                                   
046100       MOVE MFS-RENSA-FAELT          TO MOD-FLEXCP2-REFBEO-IN             
046200     ELSE                                                                 
046600       MOVE MID-FLEXCP2-REFBEO-IN    TO MOD-FLEXCP2-REFBEO-IN             
046800       MOVE MFS-ADD-LAES-IN-FAELT    TO MOD-FLEXCP2-REFBEO-IN-ATTR        
046900     END-IF                                                               
047000                                                                          
047100     IF MID-FLEXCP3-REFBEO-IN = ALL '+'                                   
047200       MOVE MFS-RENSA-FAELT          TO MOD-FLEXCP3-REFBEO-IN             
047300     ELSE                                                                 
047400       IF MID-FLEXCP3-REFBEO-IN = JA                                      
047500          MOVE JA                    TO MOD-FLEXCP3-REFBEO-IN             
047600       ELSE                                                               
047700          MOVE MID-FLEXCP3-REFBEO-IN TO MOD-FLEXCP3-REFBEO-IN             
047800       END-IF                                                             
047900       MOVE MFS-ADD-LAES-IN-FAELT    TO MOD-FLEXCP3-REFBEO-IN-ATTR        
048000     END-IF                                                               
048100                                                                          
048200     IF MID-FLEXCP4-REFBEO-IN = ALL '+'                                   
048300       MOVE MFS-RENSA-FAELT          TO MOD-FLEXCP4-REFBEO-IN             
048400     ELSE                                                                 
048800       MOVE MID-FLEXCP4-REFBEO-IN    TO MOD-FLEXCP4-REFBEO-IN             
049000       MOVE MFS-ADD-LAES-IN-FAELT    TO MOD-FLEXCP4-REFBEO-IN-ATTR        
049100     END-IF                                                               
049200                                                                          
049300     IF MID-FLEXCP1-REFBER-IN = ALL '+'                                   
049400       MOVE MFS-RENSA-FAELT          TO MOD-FLEXCP1-REFBER-IN             
049500     ELSE                                                                 
049600       IF MID-FLEXCP1-REFBER-IN = JA                                      
049700          MOVE YES                   TO MOD-FLEXCP1-REFBER-IN             
049800       ELSE                                                               
049900          MOVE MID-FLEXCP1-REFBER-IN TO MOD-FLEXCP1-REFBER-IN             
050000       END-IF                                                             
050100       MOVE MFS-ADD-LAES-IN-FAELT    TO MOD-FLEXCP1-REFBER-IN-ATTR        
050200     END-IF                                                               
050300                                                                          
050400     IF MID-FLEXCP2-REFBER-IN = ALL '+'                                   
050500       MOVE MFS-RENSA-FAELT          TO MOD-FLEXCP2-REFBER-IN             
050600     ELSE                                                                 
050700       IF MID-FLEXCP2-REFBER-IN = JA                                      
050800          MOVE YES                   TO MOD-FLEXCP2-REFBER-IN             
050900       ELSE                                                               
051000          MOVE MID-FLEXCP2-REFBER-IN TO MOD-FLEXCP2-REFBER-IN             
051100       END-IF                                                             
051200       MOVE MFS-ADD-LAES-IN-FAELT    TO MOD-FLEXCP2-REFBER-IN-ATTR        
051300     END-IF                                                               
051400                                                                          
051500     IF MID-FLOVRLAGBER-IN = ALL '+'                                      
051600       MOVE MFS-RENSA-FAELT          TO MOD-FLOVRLAGBER-IN                
051700     ELSE                                                                 
051800       IF MID-FLOVRLAGBER-IN = JA OR YES                                  
051900          MOVE YES                   TO MOD-FLOVRLAGBER-IN                
052000       ELSE                                                               
052100          MOVE MID-FLOVRLAGBER-IN    TO MOD-FLOVRLAGBER-IN                
052200       END-IF                                                             
052300       MOVE MFS-ADD-LAES-IN-FAELT    TO MOD-FLOVRLAGBER-IN-ATTR           
052400     END-IF                                                               
052500                                                                          
052600     IF MID-FLARTADD-IN = ALL '+'                                         
052700       MOVE MFS-RENSA-FAELT       TO MOD-FLARTADD-IN                      
052800     ELSE                                                                 
052900       MOVE MID-FLARTADD-IN       TO MOD-FLARTADD-IN                      
053000       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLARTADD-IN-ATTR                 
053100     END-IF                                                               
                                                                                
052600     IF MID-KVINVAUT-IN = ALL '+'                                         
052700       MOVE MFS-RENSA-FAELT       TO MOD-KVINVAUT-IN                      
052800     ELSE                                                                 
052900       MOVE MID-KVINVAUT-IN       TO MOD-KVINVAUT-IN                      
053000       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVINVAUT-IN-ATTR                 
053100     END-IF                                                               
053200                                                                          
053300     IF MID-SUINVGRANS-IN = ALL '+'                                       
053400       MOVE MFS-RENSA-FAELT       TO MOD-SUINVGRANS-IN                    
053500     ELSE                                                                 
053600       MOVE MID-SUINVGRANS-IN     TO MOD-SUINVGRANS-IN                    
053700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-SUINVGRANS-IN-ATTR               
053800     END-IF                                                               
053900                                                                          
054000     IF MID-IDPRTLST-INVA-IN = ALL '+'                                    
054100       MOVE MFS-RENSA-FAELT       TO MOD-IDPRTLST-INVA-IN                 
054200     ELSE                                                                 
054300       MOVE MID-IDPRTLST-INVA-IN  TO MOD-IDPRTLST-INVA-IN                 
054400       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDPRTLST-INVA-IN-ATTR            
054500     END-IF                                                               
054600                                                                          
054700     IF MID-IDPRTLST-INVAB-IN = ALL '+'                                   
054800       MOVE MFS-RENSA-FAELT       TO MOD-IDPRTLST-INVAB-IN                
054900     ELSE                                                                 
055000       MOVE MID-IDPRTLST-INVAB-IN TO MOD-IDPRTLST-INVAB-IN                
055100       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDPRTLST-INVAB-IN-ATTR           
055200     END-IF                                                               
055300                                                                          
055400     IF MID-IDPRTLST-INL-IN = ALL '+'                                     
055500       MOVE MFS-RENSA-FAELT       TO MOD-IDPRTLST-INL-IN                  
055600     ELSE                                                                 
055700       MOVE MID-IDPRTLST-INL-IN   TO MOD-IDPRTLST-INL-IN                  
055800       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDPRTLST-INL-IN-ATTR             
055900     END-IF                                                               
056000                                                                          
056100     IF MID-IDPRTLST-INLA-IN = ALL '+'                                    
056200       MOVE MFS-RENSA-FAELT       TO MOD-IDPRTLST-INLA-IN                 
056300     ELSE                                                                 
056400       MOVE MID-IDPRTLST-INLA-IN  TO MOD-IDPRTLST-INLA-IN                 
056500       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDPRTLST-INLA-IN-ATTR            
056600     END-IF                                                               
056700                                                                          
056800     IF MID-KDDCSTYR-BUY-IN = ALL '+'                                     
056900       MOVE MFS-RENSA-FAELT       TO MOD-KDDCSTYR-BUY-IN                  
057000     ELSE                                                                 
057100       MOVE MID-KDDCSTYR-BUY-IN   TO MOD-KDDCSTYR-BUY-IN                  
057200       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDDCSTYR-BUY-IN-ATTR             
057300     END-IF                                                               
057400                                                                          
057500     IF MID-KDDCSTYR-KUND-IN = ALL '+'                                    
057600       MOVE MFS-RENSA-FAELT       TO MOD-KDDCSTYR-KUND-IN                 
057700     ELSE                                                                 
057800       MOVE MID-KDDCSTYR-KUND-IN  TO MOD-KDDCSTYR-KUND-IN                 
057900       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDDCSTYR-KUND-IN-ATTR            
058000     END-IF                                                               
058100                                                                          
058200     IF MID-KDDCSTYR-REFTAB-IN = ALL '+'                                  
058300       MOVE MFS-RENSA-FAELT       TO MOD-KDDCSTYR-REFTAB-IN               
058400     ELSE                                                                 
058500       MOVE MID-KDDCSTYR-REFTAB-IN TO MOD-KDDCSTYR-REFTAB-IN              
058600       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDDCSTYR-REFTAB-IN-ATTR          
058700     END-IF                                                               
058800                                                                          
058900     IF MID-KVDAGAR-POKS-IN = ALL '+'                                     
059000       MOVE MFS-RENSA-FAELT       TO MOD-KVDAGAR-POKS-IN                  
059100     ELSE                                                                 
059200       MOVE MID-KVDAGAR-POKS-IN TO MOD-KVDAGAR-POKS-IN                    
059300       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVDAGAR-POKS-IN-ATTR             
059400     END-IF                                                               
059500                                                                          
059600     IF MID-KVDAGAR-PP-IN = ALL '+'                                       
059700       MOVE MFS-RENSA-FAELT       TO MOD-KVDAGAR-PP-IN                    
059800     ELSE                                                                 
059900       MOVE MID-KVDAGAR-PP-IN     TO MOD-KVDAGAR-PP-IN                    
060000       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVDAGAR-PP-IN-ATTR               
060100     END-IF                                                               
060200                                                                          
060300     IF MID-KDAKDISP-DAG-IN = ALL '+'                                     
060400       MOVE MFS-RENSA-FAELT       TO MOD-KDAKDISP-DAG-IN                  
060500     ELSE                                                                 
060600       MOVE MID-KDAKDISP-DAG-IN   TO MOD-KDAKDISP-DAG-IN                  
060700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDAKDISP-DAG-IN-ATTR             
060800     END-IF                                                               
060900                                                                          
061000     IF MID-KDAKDISP-BULK-IN = ALL '+'                                    
061100       MOVE MFS-RENSA-FAELT       TO MOD-KDAKDISP-BULK-IN                 
061200     ELSE                                                                 
061300       MOVE MID-KDAKDISP-BULK-IN  TO MOD-KDAKDISP-BULK-IN                 
061400       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDAKDISP-BULK-IN-ATTR            
061500     END-IF                                                               
061600                                                                          
061700     IF MID-FLCLEAR-BULK-IN = ALL '+'                                     
061800       MOVE MFS-RENSA-FAELT       TO MOD-FLCLEAR-BULK-IN                  
061900     ELSE                                                                 
062000       MOVE MID-FLCLEAR-BULK-IN   TO MOD-FLCLEAR-BULK-IN                  
062100       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLCLEAR-BULK-IN-ATTR             
062200     END-IF                                                               
062300                                                                          
062400     IF MID-KDSKRMET-IN = ALL '+'                                         
062500       MOVE MFS-RENSA-FAELT       TO MOD-KDSKRMET-IN                      
062600     ELSE                                                                 
062700       MOVE MID-KDSKRMET-IN       TO MOD-KDSKRMET-IN                      
062800       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDSKRMET-IN-ATTR                 
062900     END-IF                                                               
063000                                                                          
063100     IF MID-KVDAGAR-CROSS-IN = ALL '+'                                    
063200       MOVE MFS-RENSA-FAELT       TO MOD-KVDAGAR-CROSS-IN                 
063300     ELSE                                                                 
063400       MOVE MID-KVDAGAR-CROSS-IN  TO MOD-KVDAGAR-CROSS-IN                 
063500       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVDAGAR-CROSS-IN-ATTR            
063600     END-IF                                                               
063700                                                                          
063800     IF MID-REQXBRYT-IN = ALL '+'                                         
063900       MOVE MFS-RENSA-FAELT       TO MOD-REQXBRYT-IN                      
064000     ELSE                                                                 
064100       MOVE MID-REQXBRYT-IN       TO MOD-REQXBRYT-IN                      
064200       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-REQXBRYT-IN-ATTR                 
064300     END-IF                                                               
064400                                                                          
064500     IF MID-REWILSON-IN = ALL '+'                                         
064600       MOVE MFS-RENSA-FAELT       TO MOD-REWILSON-IN                      
064700     ELSE                                                                 
064800       MOVE MID-REWILSON-IN       TO MOD-REWILSON-IN                      
064900       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-REWILSON-IN-ATTR                 
065000     END-IF                                                               
065100                                                                          
065200     IF MID-FLKNDVAL-IN = ALL '+'                                         
065300       MOVE MFS-RENSA-FAELT       TO MOD-FLKNDVAL-IN                      
065400     ELSE                                                                 
065500       IF MID-FLKNDVAL-IN = JA                                            
065600          MOVE YES                TO MOD-FLKNDVAL-IN                      
065700       ELSE                                                               
065800          MOVE MID-FLKNDVAL-IN    TO MOD-FLKNDVAL-IN                      
065900       END-IF                                                             
066000       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLKNDVAL-IN-ATTR                 
066100     END-IF                                                               
066200                                                                          
066300     IF MID-KDFAKTDC-IN = ALL '+'                                         
066400       MOVE MFS-RENSA-FAELT       TO MOD-KDFAKTDC-IN                      
066500     ELSE                                                                 
066600       MOVE MID-KDFAKTDC-IN       TO MOD-KDFAKTDC-IN                      
066700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDFAKTDC-IN-ATTR                 
066800     END-IF                                                               
066900                                                                          
067000     IF MID-IDDC-REF-IN = ALL '+'                                         
067100       MOVE MFS-RENSA-FAELT       TO MOD-IDDC-REF-IN                      
067200     ELSE                                                                 
067300       MOVE MID-IDDC-REF-IN       TO MOD-IDDC-REF-IN                      
067400       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDDC-REF-IN-ATTR                 
067500     END-IF                                                               
067600                                                                          
067700     IF MID-TID-RETOS-IN = ALL '+'                                        
067800       MOVE MFS-RENSA-FAELT       TO MOD-TID-RETOS-IN                     
067900     ELSE                                                                 
068000       MOVE MID-TID-RETOS-IN      TO MOD-TID-RETOS-IN                     
068100       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TID-RETOS-IN-ATTR                
068200     END-IF                                                               
068300                                                                          
068400     IF MID-TID-RET98-IN = ALL '+'                                        
068500       MOVE MFS-RENSA-FAELT       TO MOD-TID-RET98-IN                     
068600     ELSE                                                                 
068700       MOVE MID-TID-RET98-IN      TO MOD-TID-RET98-IN                     
068800       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TID-RET98-IN-ATTR                
068900     END-IF                                                               
069000                                                                          
069100     IF MID-PRARTSTD-SKRLO98-IN = ALL '+'                                 
069200       MOVE MFS-RENSA-FAELT       TO MOD-PRARTSTD-SKRLO98-IN              
069300     ELSE                                                                 
069400       MOVE MID-PRARTSTD-SKRLO98-IN TO MOD-PRARTSTD-SKRLO98-IN            
069500       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-PRARTSTD-SKRLO98-IN-ATTR        
069600     END-IF                                                               
069700                                                                          
069800     IF MID-FLTRLO88-MAN-IN = ALL '+'                                     
069900       MOVE MFS-RENSA-FAELT       TO MOD-FLTRLO88-MAN-IN                  
070000     ELSE                                                                 
070100       MOVE MID-FLTRLO88-MAN-IN   TO MOD-FLTRLO88-MAN-IN                  
070200       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLTRLO88-MAN-IN-ATTR             
070300     END-IF                                                               
070400                                                                          
070500     IF MID-FLTRLO88-TIS-IN = ALL '+'                                     
070600       MOVE MFS-RENSA-FAELT       TO MOD-FLTRLO88-TIS-IN                  
070700     ELSE                                                                 
070800       MOVE MID-FLTRLO88-TIS-IN   TO MOD-FLTRLO88-TIS-IN                  
070900       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLTRLO88-TIS-IN-ATTR             
071000     END-IF                                                               
071100                                                                          
071200     IF MID-FLTRLO88-ONS-IN = ALL '+'                                     
071300       MOVE MFS-RENSA-FAELT       TO MOD-FLTRLO88-ONS-IN                  
071400     ELSE                                                                 
071500       MOVE MID-FLTRLO88-ONS-IN   TO MOD-FLTRLO88-ONS-IN                  
071600       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLTRLO88-ONS-IN-ATTR             
071700     END-IF                                                               
071800                                                                          
071900     IF MID-FLTRLO88-TOR-IN = ALL '+'                                     
072000       MOVE MFS-RENSA-FAELT       TO MOD-FLTRLO88-TOR-IN                  
072100     ELSE                                                                 
072200       MOVE MID-FLTRLO88-TOR-IN   TO MOD-FLTRLO88-TOR-IN                  
072300       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLTRLO88-TOR-IN-ATTR             
072400     END-IF                                                               
072500                                                                          
072600     IF MID-FLTRLO88-FRE-IN = ALL '+'                                     
072700       MOVE MFS-RENSA-FAELT       TO MOD-FLTRLO88-FRE-IN                  
072800     ELSE                                                                 
072900       MOVE MID-FLTRLO88-FRE-IN   TO MOD-FLTRLO88-FRE-IN                  
073000       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLTRLO88-FRE-IN-ATTR             
073100     END-IF                                                               
073200                                                                          
073300     .                                                                    
073400     EJECT                                                                
073500 F-LAES-VISA-INFO SECTION.                                                
073600                                                                          
073700     MOVE MSGI-IDDC-KEY TO W-IDDC                                         
073800                                                                          
073900     PERFORM IMS-GU-WDB601                                                
074000                                                                          
074100     IF SEGMENT-SAKNAS                                                    
074200       MOVE ERR-DC-MISSING TO MED-IDMFSFEL                                
074300       CALL WMEDKONV USING MED-WMEDAREA                                   
074400       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
074500       PERFORM MFS-RENSA-FAELT-UT                                         
074600     ELSE                                                                 
074700       IF DCS-FLDCRET = JA                                                
074800          MOVE YES              TO MOD-FLDCRET                            
074900       ELSE                                                               
075000          MOVE DCS-FLDCRET      TO MOD-FLDCRET                            
075100       END-IF                                                             
075200                                                                          
075300       IF DCS-FLINLHIST = JA                                              
075400          MOVE YES              TO MOD-FLINLHIST                          
075500       ELSE                                                               
075600          MOVE DCS-FLINLHIST    TO MOD-FLINLHIST                          
075700       END-IF                                                             
075800                                                                          
075900       IF DCS-FLFSEDEL = JA                                               
076000          MOVE YES              TO MOD-FLFSEDEL                           
076100       ELSE                                                               
076200          MOVE DCS-FLFSEDEL     TO MOD-FLFSEDEL                           
076300       END-IF                                                             
076400                                                                          
076500       IF DCS-FLINLREP = JA                                               
076600          MOVE YES              TO MOD-FLINLREP                           
076700       ELSE                                                               
076800          MOVE DCS-FLINLREP     TO MOD-FLINLREP                           
076900       END-IF                                                             
077000                                                                          
077100       IF DCS-FLPRISSPR = JA                                              
077200          MOVE YES              TO MOD-FLPRISSPR                          
077300       ELSE                                                               
077400          MOVE DCS-FLPRISSPR    TO MOD-FLPRISSPR                          
077500       END-IF                                                             
077600                                                                          
077700       IF DCS-FLBINNUT = JA                                               
077800          MOVE YES              TO MOD-FLBINNUT                           
077900       ELSE                                                               
078000          MOVE DCS-FLBINNUT     TO MOD-FLBINNUT                           
078100       END-IF                                                             
078200                                                                          
078300       IF DCS-FLSAMPAK = JA                                               
078400          MOVE YES              TO MOD-FLSAMPAK                           
078500       ELSE                                                               
078600          MOVE DCS-FLSAMPAK     TO MOD-FLSAMPAK                           
078700       END-IF                                                             
078800                                                                          
078900       IF DCS-FLSEASBER = JA                                              
079000          MOVE YES              TO MOD-FLSEASBER                          
079100       ELSE                                                               
079200          MOVE DCS-FLSEASBER    TO MOD-FLSEASBER                          
079300       END-IF                                                             
079400                                                                          
079500       IF DCS-FLTYP6JU = JA                                               
079600          MOVE YES              TO MOD-FLTYP6JU                           
079700       ELSE                                                               
079800          MOVE DCS-FLTYP6JU     TO MOD-FLTYP6JU                           
079900       END-IF                                                             
080000                                                                          
080100       IF DCS-FLRSI = JA                                                  
080200          MOVE YES                TO MOD-FLRSI                            
080300       ELSE                                                               
080400          MOVE DCS-FLRSI        TO MOD-FLRSI                              
080500       END-IF                                                             
080600                                                                          
080700       IF DCS-KDPORDL = JA                                                
080800          MOVE YES              TO MOD-KDPORDL                            
080900       ELSE                                                               
081000          MOVE DCS-KDPORDL      TO MOD-KDPORDL                            
081100       END-IF                                                             
081200                                                                          
081300       MOVE DCS-IDLISTNR        TO MOD-IDLISTNR                           
081400       IF DCS-FLEXCP1-PRIO = JA                                           
081500          MOVE YES              TO MOD-FLEXCP1-PRIO                       
081600       ELSE                                                               
081700          MOVE DCS-FLEXCP1-PRIO TO MOD-FLEXCP1-PRIO                       
081800       END-IF                                                             
081900                                                                          
082300       MOVE DCS-FLEXCP1-REFBEO    TO MOD-FLEXCP1-REFBEO                   
082500                                                                          
082900       MOVE DCS-FLEXCP2-REFBEO    TO MOD-FLEXCP2-REFBEO                   
083100                                                                          
083200       IF DCS-FLEXCP3-REFBEO = JA                                         
083300          MOVE YES                TO MOD-FLEXCP3-REFBEO                   
083400       ELSE                                                               
083500          MOVE DCS-FLEXCP3-REFBEO TO MOD-FLEXCP3-REFBEO                   
083600       END-IF                                                             
083700                                                                          
084100       MOVE DCS-FLEXCP4-REFBEO    TO MOD-FLEXCP4-REFBEO                   
084300                                                                          
084400       IF DCS-FLEXCP1-REFBER = JA                                         
084500          MOVE YES                TO MOD-FLEXCP1-REFBER                   
084600       ELSE                                                               
084700          MOVE DCS-FLEXCP1-REFBER TO MOD-FLEXCP1-REFBER                   
084800       END-IF                                                             
084900                                                                          
085000       IF DCS-FLEXCP2-REFBER = JA                                         
085100          MOVE YES                TO MOD-FLEXCP2-REFBER                   
085200       ELSE                                                               
085300          MOVE DCS-FLEXCP2-REFBER TO MOD-FLEXCP2-REFBER                   
085400       END-IF                                                             
085500                                                                          
085600       IF DCS-FLOVRLAGBER = JA                                            
085700*         --- DEFAULT-VÄRDET, SOM BETYDER ATT ALL TILLGÅNG RÄKNAS         
085800          MOVE YES                TO MOD-FLOVRLAGBER                      
085900       ELSE                                                               
086000*         --- N BETYDER ATT ÖVERLAGERSALDO EJ SKALL MEDRÄKNAS             
086100          MOVE DCS-FLOVRLAGBER    TO MOD-FLOVRLAGBER                      
086200       END-IF                                                             
086300                                                                          
086400       MOVE DCS-FLARTADD        TO MOD-FLARTADD                           
086400       MOVE DCS-KVINVAUT        TO MOD-KVINVAUT                           
086500       MOVE DCS-SUINVGRANS      TO MOD-SUINVGRANS                         
086600       MOVE DCS-IDPRTLST-INVA   TO MOD-IDPRTLST-INVA                      
086700       MOVE DCS-IDPRTLST-INVAB  TO MOD-IDPRTLST-INVAB                     
086800       MOVE DCS-IDPRTLST-INL    TO MOD-IDPRTLST-INL                       
086900       MOVE DCS-IDPRTLST-INLA   TO MOD-IDPRTLST-INLA                      
087000       MOVE DCS-KDDCSTYR-BUY    TO MOD-KDDCSTYR-BUY                       
087100       MOVE DCS-KDDCSTYR-KUND   TO MOD-KDDCSTYR-KUND                      
087200       MOVE DCS-KDDCSTYR-REFTAB TO MOD-KDDCSTYR-REFTAB                    
087300       MOVE DCS-KVDAGAR-POKS    TO MOD-KVDAGAR-POKS                       
087400       MOVE DCS-KVDAGAR-PP      TO MOD-KVDAGAR-PP                         
087500       MOVE DCS-KDAKDISP-DAG    TO MOD-KDAKDISP-DAG                       
087600       MOVE DCS-KDAKDISP-BULK   TO MOD-KDAKDISP-BULK                      
087700       IF DCS-FLCLEAR-BULK = JA OR YES                                    
087800          MOVE YES              TO MOD-FLCLEAR-BULK                       
087900       ELSE                                                               
088000          MOVE NOO              TO MOD-FLCLEAR-BULK                       
088100       END-IF                                                             
088200       MOVE DCS-KDSKRMET        TO MOD-KDSKRMET                           
088300       MOVE DCS-KVDAGAR-CROSS   TO MOD-KVDAGAR-CROSS                      
088400       MOVE DCS-REQXBRYT        TO MOD-REQXBRYT                           
088500       MOVE DCS-REWILSON        TO MOD-REWILSON                           
088600       IF DCS-FLKNDVAL = JA                                               
088700          MOVE YES              TO MOD-FLKNDVAL                           
088800       ELSE                                                               
088900          MOVE DCS-FLKNDVAL     TO MOD-FLKNDVAL                           
089000       END-IF                                                             
089100       MOVE DCS-KDFAKTDC        TO MOD-KDFAKTDC                           
089200       MOVE DCS-IDDC-REF        TO MOD-IDDC-REF                           
089300       EVALUATE DCS-TID-RETOS                                             
089400         WHEN ZERO                                                        
089500           MOVE NEJ             TO MOD-TID-RETOS                          
089600         WHEN 7                                                           
089700           MOVE 'MO'            TO MOD-TID-RETOS                          
089800         WHEN 2                                                           
089900           MOVE 'TU'            TO MOD-TID-RETOS                          
090000         WHEN 3                                                           
090100           MOVE 'WE'            TO MOD-TID-RETOS                          
090200         WHEN 4                                                           
090300           MOVE 'TH'            TO MOD-TID-RETOS                          
090400         WHEN 5                                                           
090500           MOVE 'FR'            TO MOD-TID-RETOS                          
090600       END-EVALUATE                                                       
090700                                                                          
090800       EVALUATE DCS-TID-RET98                                             
090900         WHEN ZERO                                                        
091000           MOVE NEJ             TO MOD-TID-RET98                          
091100         WHEN 7                                                           
091200           MOVE 'MO'            TO MOD-TID-RET98                          
091300         WHEN 2                                                           
091400           MOVE 'TU'            TO MOD-TID-RET98                          
091500         WHEN 3                                                           
091600           MOVE 'WE'            TO MOD-TID-RET98                          
091700         WHEN 4                                                           
091800           MOVE 'TH'            TO MOD-TID-RET98                          
091900         WHEN 5                                                           
092000           MOVE 'FR'            TO MOD-TID-RET98                          
092100       END-EVALUATE                                                       
092200                                                                          
092300       MOVE DCS-PRARTSTD-SKRLO98  TO MOD-PRARTSTD-SKRLO98                 
092400                                                                          
092500       IF DCS-FLTRLO88-MAN    = 'Y'                                       
092600           MOVE YES             TO MOD-FLTRLO88-MAN                       
092700       ELSE                                                               
092800           MOVE NEJ             TO MOD-FLTRLO88-MAN                       
092900       END-IF                                                             
093000                                                                          
093100       IF DCS-FLTRLO88-TIS    = 'Y'                                       
093200           MOVE YES             TO MOD-FLTRLO88-TIS                       
093300       ELSE                                                               
093400           MOVE NEJ             TO MOD-FLTRLO88-TIS                       
093500       END-IF                                                             
093600                                                                          
093700       IF DCS-FLTRLO88-ONS    = 'Y'                                       
093800           MOVE YES             TO MOD-FLTRLO88-ONS                       
093900       ELSE                                                               
094000           MOVE NEJ             TO MOD-FLTRLO88-ONS                       
094100       END-IF                                                             
094200                                                                          
094300       IF DCS-FLTRLO88-TOR    = 'Y'                                       
094400           MOVE YES             TO MOD-FLTRLO88-TOR                       
094500       ELSE                                                               
094600           MOVE NEJ             TO MOD-FLTRLO88-TOR                       
094700       END-IF                                                             
094800                                                                          
094900       IF DCS-FLTRLO88-FRE    = 'Y'                                       
095000           MOVE YES             TO MOD-FLTRLO88-FRE                       
095100       ELSE                                                               
095200           MOVE NEJ             TO MOD-FLTRLO88-FRE                       
095300       END-IF                                                             
095400                                                                          
095500     END-IF                                                               
095600     .                                                                    
095700     EJECT                                                                
095800 G-KOLLA-INPUT SECTION.                                                   
095900                                                                          
096000     MOVE JA  TO INDATA-SW                                                
096100     IF MID-INPUT = ALL '+'                                               
096200       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
096300       CALL WMEDKONV USING MED-WMEDAREA                                   
096400       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
096500       PERFORM MFS-ROER-EJ-FAELT-IN                                       
096600       PERFORM MFS-ROER-EJ-FAELT-UT                                       
096700       MOVE NEJ TO INDATA-SW                                              
096800     ELSE                                                                 
096900       PERFORM IMS-GU-WDB601                                              
097000       IF SEGMENT-FINNS                                                   
097100         PERFORM GA-KOLLA-ANDR-SEGM                                       
097200       ELSE                                                               
097300         MOVE ERR-DC-MISSING  TO MED-IDMFSFEL                             
097400         MOVE NEJ TO INDATA-SW                                            
097500       END-IF                                                             
097600                                                                          
097700                                                                          
097800       IF INDATA-FEL                                                      
097900         IF MED-IDMFSFEL = SPACE                                          
098000           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
098100         END-IF                                                           
098200         CALL WMEDKONV USING MED-WMEDAREA                                 
098300         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
098400         PERFORM MFS-ROER-EJ-FAELT-UT                                     
098500         PERFORM MFS-ROER-EJ-FAELT-IN                                     
098600       END-IF                                                             
098700     END-IF                                                               
098800     .                                                                    
098900     EJECT                                                                
099000 GA-KOLLA-ANDR-SEGM SECTION.                                              
099100                                                                          
099200     IF MID-FLDCRET-IN NOT = ALL '+'                                      
099300       IF MID-FLDCRET-IN = NEJ OR JA OR YES                               
099400         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLDCRET-IN-ATTR                 
099500       ELSE                                                               
099600         MOVE MFS-ALFA-FAELT-FEL TO MOD-FLDCRET-IN-ATTR                   
099700         MOVE NEJ TO INDATA-SW                                            
099800       END-IF                                                             
099900     ELSE                                                                 
100000       MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLDCRET-IN-ATTR                   
100100     END-IF                                                               
100200                                                                          
100300     IF MID-FLINLHIST-IN NOT = ALL '+'                                    
100400       IF MID-FLINLHIST-IN = 'J' OR 'N' OR 'Y'                            
100500         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLINLHIST-IN-ATTR               
100600       ELSE                                                               
100700         MOVE MFS-ALFA-FAELT-FEL TO MOD-FLINLHIST-IN-ATTR                 
100800         MOVE NEJ TO INDATA-SW                                            
100900       END-IF                                                             
101000     ELSE                                                                 
101100       MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLINLHIST-IN-ATTR                 
101200     END-IF                                                               
101300                                                                          
101400     IF MID-FLFSEDEL-IN NOT = ALL '+'                                     
101500       IF MID-FLFSEDEL-IN = 'J' OR 'N' OR 'Y'                             
101600         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLFSEDEL-IN-ATTR                
101700       ELSE                                                               
101800         MOVE MFS-ALFA-FAELT-FEL TO MOD-FLFSEDEL-IN-ATTR                  
101900         MOVE NEJ TO INDATA-SW                                            
102000       END-IF                                                             
102100     ELSE                                                                 
102200       MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLFSEDEL-IN-ATTR                  
102300     END-IF                                                               
102400                                                                          
102500     IF MID-FLINLREP-IN NOT = ALL '+'                                     
102600       IF MID-FLINLREP-IN = 'J' OR 'N' OR 'Y'                             
102700         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLINLREP-IN-ATTR                
102800       ELSE                                                               
102900         MOVE MFS-ALFA-FAELT-FEL TO MOD-FLINLREP-IN-ATTR                  
103000         MOVE NEJ TO INDATA-SW                                            
103100       END-IF                                                             
103200     ELSE                                                                 
103300       MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLINLREP-IN-ATTR                  
103400     END-IF                                                               
103500                                                                          
103600     IF MID-FLPRISSPR-IN NOT = ALL '+'                                    
103700       IF MID-FLPRISSPR-IN = 'J' OR 'N' OR 'Y'                            
103800         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLPRISSPR-IN-ATTR               
103900       ELSE                                                               
104000         MOVE MFS-ALFA-FAELT-FEL TO MOD-FLPRISSPR-IN-ATTR                 
104100         MOVE NEJ TO INDATA-SW                                            
104200       END-IF                                                             
104300     ELSE                                                                 
104400       MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLPRISSPR-IN-ATTR                 
104500     END-IF                                                               
104600                                                                          
104700     IF MID-FLBINNUT-IN NOT = ALL '+'                                     
104800       IF MID-FLBINNUT-IN = 'J' OR 'N' OR 'Y'                             
104900         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLBINNUT-IN-ATTR                
105000       ELSE                                                               
105100         MOVE MFS-ALFA-FAELT-FEL TO MOD-FLBINNUT-IN-ATTR                  
105200         MOVE NEJ TO INDATA-SW                                            
105300       END-IF                                                             
105400     ELSE                                                                 
105500       MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLBINNUT-IN-ATTR                  
105600     END-IF                                                               
105700                                                                          
105800     IF MID-FLSAMPAK-IN NOT = ALL '+'                                     
105900       IF MID-FLSAMPAK-IN = 'J' OR 'N' OR 'Y'                             
106000         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLSAMPAK-IN-ATTR                
106100       ELSE                                                               
106200         MOVE MFS-ALFA-FAELT-FEL TO MOD-FLSAMPAK-IN-ATTR                  
106300         MOVE NEJ TO INDATA-SW                                            
106400       END-IF                                                             
106500     ELSE                                                                 
106600       MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLSAMPAK-IN-ATTR                  
106700     END-IF                                                               
106800                                                                          
106900     IF MID-FLSEASBER-IN NOT = ALL '+'                                    
107000       IF MID-FLSEASBER-IN = 'J' OR 'N' OR 'Y'                            
107100         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLSEASBER-IN-ATTR               
107200       ELSE                                                               
107300         MOVE MFS-ALFA-FAELT-FEL TO MOD-FLSEASBER-IN-ATTR                 
107400         MOVE NEJ TO INDATA-SW                                            
107500       END-IF                                                             
107600     ELSE                                                                 
107700       MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLSEASBER-IN-ATTR                 
107800     END-IF                                                               
107900                                                                          
108000     IF MID-FLTYP6JU-IN NOT = ALL '+'                                     
108100       IF MID-FLTYP6JU-IN = 'J' OR 'N' OR 'Y'                             
108200         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLTYP6JU-IN-ATTR                
108300       ELSE                                                               
108400         MOVE MFS-ALFA-FAELT-FEL TO MOD-FLTYP6JU-IN-ATTR                  
108500         MOVE NEJ TO INDATA-SW                                            
108600       END-IF                                                             
108700     ELSE                                                                 
108800       MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLTYP6JU-IN-ATTR                  
108900     END-IF                                                               
109000                                                                          
109100     IF MID-FLRSI-IN NOT = ALL '+'                                        
109200       IF MID-FLRSI-IN = 'J' OR 'N' OR 'Y'                                
109300         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLRSI-IN-ATTR                   
109400       ELSE                                                               
109500         MOVE MFS-ALFA-FAELT-FEL TO MOD-FLRSI-IN-ATTR                     
109600         MOVE NEJ TO INDATA-SW                                            
109700       END-IF                                                             
109800     ELSE                                                                 
109900       MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLRSI-IN-ATTR                     
110000     END-IF                                                               
110100                                                                          
110200     IF MID-KDPORDL-IN NOT = ALL '+'                                      
110300       IF MID-KDPORDL-IN = 'J' OR 'N' OR 'Y'                              
110400         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDPORDL-IN-ATTR                 
110500       ELSE                                                               
110600         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDPORDL-IN-ATTR                   
110700         MOVE NEJ TO INDATA-SW                                            
110800       END-IF                                                             
110900     ELSE                                                                 
111000       MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDPORDL-IN-ATTR                   
111100     END-IF                                                               
111200                                                                          
111300     IF MID-IDLISTNR-IN NOT = ALL '+'                                     
111400       IF MID-IDLISTNR-IN NUMERIC                                         
111500         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDLISTNR-IN-ATTR                 
111600       ELSE                                                               
111700         MOVE MFS-NUM-FAELT-FEL TO MOD-IDLISTNR-IN-ATTR                   
111800         MOVE NEJ TO INDATA-SW                                            
111900       END-IF                                                             
112000     ELSE                                                                 
112100       MOVE MFS-NUM-FAELT-RAETT TO MOD-IDLISTNR-IN-ATTR                   
112200     END-IF                                                               
112300                                                                          
112400     IF MID-FLEXCP1-PRIO-IN NOT = ALL '+'                                 
112500       IF MID-FLEXCP1-PRIO-IN = 'J' OR 'N' OR 'Y'                         
112600         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLEXCP1-PRIO-IN-ATTR            
112700       ELSE                                                               
112800         MOVE MFS-ALFA-FAELT-FEL TO MOD-FLEXCP1-PRIO-IN-ATTR              
112900         MOVE NEJ TO INDATA-SW                                            
113000       END-IF                                                             
113100     ELSE                                                                 
113200       MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLEXCP1-PRIO-IN-ATTR              
113300     END-IF                                                               
113400                                                                          
113500     IF MID-FLEXCP1-REFBEO-IN NOT = ALL '+'                               
113600       IF MID-FLEXCP1-REFBEO-IN IS NUMERIC                                
113700         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLEXCP1-REFBEO-IN-ATTR          
113800       ELSE                                                               
113900         MOVE MFS-ALFA-FAELT-FEL TO MOD-FLEXCP1-REFBEO-IN-ATTR            
114000         MOVE NEJ TO INDATA-SW                                            
114100       END-IF                                                             
114200     ELSE                                                                 
114300       MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLEXCP1-REFBEO-IN-ATTR            
114400     END-IF                                                               
114500                                                                          
114600     IF MID-FLEXCP2-REFBEO-IN NOT = ALL '+'                               
114700       IF MID-FLEXCP2-REFBEO-IN IS NUMERIC                                
114800         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLEXCP2-REFBEO-IN-ATTR          
114900       ELSE                                                               
115000         MOVE MFS-ALFA-FAELT-FEL TO MOD-FLEXCP2-REFBEO-IN-ATTR            
115100         MOVE NEJ TO INDATA-SW                                            
115200       END-IF                                                             
115300     ELSE                                                                 
115400       MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLEXCP2-REFBEO-IN-ATTR            
115500     END-IF                                                               
115600                                                                          
115700     IF MID-FLEXCP3-REFBEO-IN NOT = ALL '+'                               
115800       IF MID-FLEXCP3-REFBEO-IN = 'J' OR 'N' OR 'Y'                       
115900         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLEXCP3-REFBEO-IN-ATTR          
116000       ELSE                                                               
116100         MOVE MFS-ALFA-FAELT-FEL TO MOD-FLEXCP3-REFBEO-IN-ATTR            
116200         MOVE NEJ TO INDATA-SW                                            
116300       END-IF                                                             
116400     ELSE                                                                 
116500       MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLEXCP3-REFBEO-IN-ATTR            
116600     END-IF                                                               
116700                                                                          
116800     IF MID-FLEXCP4-REFBEO-IN NOT = ALL '+'                               
116900       IF MID-FLEXCP4-REFBEO-IN IS NUMERIC                                
117000         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLEXCP4-REFBEO-IN-ATTR          
117100       ELSE                                                               
117200         MOVE MFS-ALFA-FAELT-FEL TO MOD-FLEXCP4-REFBEO-IN-ATTR            
117300         MOVE NEJ TO INDATA-SW                                            
117400       END-IF                                                             
117500     ELSE                                                                 
117600       MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLEXCP4-REFBEO-IN-ATTR            
117700     END-IF                                                               
117800                                                                          
117900     IF MID-FLEXCP1-REFBER-IN NOT = ALL '+'                               
118000       IF MID-FLEXCP1-REFBER-IN = 'J' OR 'N' OR 'Y'                       
118100         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLEXCP1-REFBER-IN-ATTR          
118200       ELSE                                                               
118300         MOVE MFS-ALFA-FAELT-FEL TO MOD-FLEXCP1-REFBER-IN-ATTR            
118400         MOVE NEJ TO INDATA-SW                                            
118500       END-IF                                                             
118600     ELSE                                                                 
118700       MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLEXCP1-REFBER-IN-ATTR            
118800     END-IF                                                               
118900                                                                          
119000     IF MID-FLEXCP2-REFBER-IN NOT = ALL '+'                               
119100       IF MID-FLEXCP2-REFBER-IN = 'J' OR 'N' OR 'Y'                       
119200         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLEXCP2-REFBER-IN-ATTR          
119300       ELSE                                                               
119400         MOVE MFS-ALFA-FAELT-FEL TO MOD-FLEXCP2-REFBER-IN-ATTR            
119500         MOVE NEJ TO INDATA-SW                                            
119600       END-IF                                                             
119700     ELSE                                                                 
119800       MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLEXCP2-REFBER-IN-ATTR            
119900     END-IF                                                               
120000                                                                          
119000     IF MID-FLARTADD-IN NOT = ALL '+'                                     
119100       IF MID-FLARTADD-IN = 'J' OR 'N' OR 'Y'                             
119200         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLARTADD-IN-ATTR                
119300       ELSE                                                               
119400         MOVE MFS-ALFA-FAELT-FEL TO MOD-FLARTADD-IN-ATTR                  
119500         MOVE NEJ TO INDATA-SW                                            
119600       END-IF                                                             
119700     ELSE                                                                 
119800       MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLARTADD-IN-ATTR                  
119900     END-IF                                                               
                                                                                
120100     IF MID-FLOVRLAGBER-IN NOT = ALL '+'                                  
120200       IF MID-FLOVRLAGBER-IN = JA  OR NEJ OR YES                          
120300         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLOVRLAGBER-IN-ATTR             
120400       ELSE                                                               
120500         MOVE MFS-ALFA-FAELT-FEL TO MOD-FLOVRLAGBER-IN-ATTR               
120600         MOVE NEJ TO INDATA-SW                                            
120700       END-IF                                                             
120800     ELSE                                                                 
120900       MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLOVRLAGBER-IN-ATTR               
121000     END-IF                                                               
121100                                                                          
121200     IF MID-IDPRTLST-INVA-IN NOT = ALL '+'                                
121300         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDPRTLST-INVA-IN-ATTR            
121400     END-IF                                                               
121500                                                                          
121600     IF MID-KVINVAUT-IN NOT = ALL '+'                                     
121700       IF MID-KVINVAUT-IN NUMERIC                                         
121800         IF NOT DCS-NDC-NA                                                
121900           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVINVAUT-IN-ATTR               
122000         ELSE                                                             
122100           MOVE MFS-ALFA-FAELT-FEL  TO MOD-KVINVAUT-IN-ATTR               
122200           MOVE ERR-UPD-NOT-ALLOWED TO MED-IDMFSFEL                       
122300           MOVE NEJ                 TO INDATA-SW                          
122400         END-IF                                                           
122500       ELSE                                                               
122600           MOVE MFS-NUM-FAELT-FEL   TO MOD-KVINVAUT-IN-ATTR               
122700           MOVE NEJ                 TO INDATA-SW                          
122800       END-IF                                                             
122900     END-IF                                                               
123000                                                                          
123100     IF MID-SUINVGRANS-IN NOT = ALL '+'                                   
123200       IF MID-SUINVGRANS-IN NUMERIC                                       
123300           MOVE MFS-NUM-FAELT-RAETT TO MOD-SUINVGRANS-IN-ATTR             
123400       ELSE                                                               
123500           MOVE MFS-NUM-FAELT-FEL   TO MOD-SUINVGRANS-IN-ATTR             
123600           MOVE NEJ                 TO INDATA-SW                          
123700       END-IF                                                             
123800     END-IF                                                               
123900                                                                          
124000     IF MID-IDPRTLST-INVAB-IN NOT = ALL '+'                               
124100         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDPRTLST-INVAB-IN-ATTR           
124200     END-IF                                                               
124300                                                                          
124400     IF MID-IDPRTLST-INL-IN NOT = ALL '+'                                 
124500         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDPRTLST-INL-IN-ATTR             
124600     END-IF                                                               
124700                                                                          
124800     IF MID-IDPRTLST-INLA-IN NOT = ALL '+'                                
124900         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDPRTLST-INLA-IN-ATTR            
125000     END-IF                                                               
125100                                                                          
125200     IF MID-KDDCSTYR-BUY-IN NOT = ALL '+'                                 
125300       IF MID-KDDCSTYR-BUY-IN NUMERIC                                     
125400         MOVE MFS-NUM-FAELT-RAETT TO MOD-KDDCSTYR-BUY-IN-ATTR             
125500*                                                                         
125600         IF MID-KDDCSTYR-BUY-IN = ZERO                                    
125700            IF MID-KDDCSTYR-REFTAB-IN NUMERIC                             
125710               IF MID-KDDCSTYR-REFTAB-IN > ZERO                           
125720                  MOVE 'IF BUY-STEER ZERO,REF-STEER MUST BE ZERO'         
125730                                                TO MOD-TEMFSINF           
125740                  MOVE MFS-NUM-FAELT-FEL TO                               
125750                                    MOD-KDDCSTYR-REFTAB-IN-ATTR           
125760                  MOVE NEJ TO INDATA-SW                                   
125770               END-IF                                                     
125780            ELSE                                                          
125790               MOVE ZERO TO MID-KDDCSTYR-REFTAB-IN                        
125791            END-IF                                                        
125800         ELSE                                                             
126400            IF DCS-KDDCSTYR-REFTAB = ZERO AND                             
126410               MID-KDDCSTYR-REFTAB-IN NOT NUMERIC                         
126500               MOVE 'REF-STEER ALSO SHOULD BE CHANGED'                    
126600                                             TO MOD-TEMFSINF              
126700               MOVE MFS-NUM-FAELT-FEL TO                                  
126800                                 MOD-KDDCSTYR-REFTAB-IN-ATTR              
126900               MOVE NEJ TO INDATA-SW                                      
127000            END-IF                                                        
127300         END-IF                                                           
127310*                                                                         
127400       ELSE                                                               
127500         MOVE MFS-NUM-FAELT-FEL TO MOD-KDDCSTYR-BUY-IN-ATTR               
127600         MOVE NEJ TO INDATA-SW                                            
127700       END-IF                                                             
127800     END-IF                                                               
127900                                                                          
128000     IF MID-KDDCSTYR-KUND-IN NOT = ALL '+'                                
128100       IF MID-KDDCSTYR-KUND-IN NUMERIC                                    
128200         MOVE MFS-NUM-FAELT-RAETT TO MOD-KDDCSTYR-KUND-IN-ATTR            
128300       ELSE                                                               
128400         MOVE MFS-NUM-FAELT-FEL TO MOD-KDDCSTYR-KUND-IN-ATTR              
128500         MOVE NEJ TO INDATA-SW                                            
128600       END-IF                                                             
128700     END-IF                                                               
128800                                                                          
128900     IF MID-KDDCSTYR-REFTAB-IN NOT = ALL '+'                              
129000       IF MID-KDDCSTYR-REFTAB-IN NUMERIC                                  
129100         MOVE MFS-NUM-FAELT-RAETT TO MOD-KDDCSTYR-REFTAB-IN-ATTR          
129200*                                                                         
129300         IF MID-KDDCSTYR-REFTAB-IN = ZERO                                 
129400            IF MID-KDDCSTYR-BUY-IN NUMERIC                                
129500               IF MID-KDDCSTYR-BUY-IN > ZERO                              
129600                  MOVE 'IF REF-STEER ZERO,BUY-STEER MUST BE ZERO'         
129700                                                TO MOD-TEMFSINF           
129800                  MOVE MFS-NUM-FAELT-FEL TO                               
129900                                    MOD-KDDCSTYR-BUY-IN-ATTR              
130000                  MOVE NEJ TO INDATA-SW                                   
130100               END-IF                                                     
130200            ELSE                                                          
130300               MOVE ZERO TO MID-KDDCSTYR-BUY-IN                           
131000            END-IF                                                        
131010         ELSE                                                             
131070            IF DCS-KDDCSTYR-BUY    = ZERO AND                             
131071               MID-KDDCSTYR-BUY-IN NOT NUMERIC                            
131080               MOVE 'BUY-STEER ALSO SHOULD BE CHANGED'                    
131090                                           TO MOD-TEMFSINF                
131091               MOVE MFS-NUM-FAELT-FEL TO                                  
131092                                    MOD-KDDCSTYR-BUY-IN-ATTR              
131093               MOVE NEJ TO INDATA-SW                                      
131095            END-IF                                                        
131100         END-IF                                                           
131200*                                                                         
131300       ELSE                                                               
131400         MOVE MFS-NUM-FAELT-FEL TO MOD-KDDCSTYR-REFTAB-IN-ATTR            
131500         MOVE NEJ TO INDATA-SW                                            
131600       END-IF                                                             
131700     END-IF                                                               
131800                                                                          
131900     IF MID-KVDAGAR-POKS-IN NOT = ALL '+'                                 
132000       IF MID-KVDAGAR-POKS-IN NUMERIC                                     
132100         MOVE MFS-NUM-FAELT-RAETT TO MOD-KVDAGAR-POKS-IN-ATTR             
132200       ELSE                                                               
132300         MOVE MFS-NUM-FAELT-FEL TO MOD-KVDAGAR-POKS-IN-ATTR               
132400         MOVE NEJ TO INDATA-SW                                            
132500       END-IF                                                             
132600     END-IF                                                               
132700                                                                          
132800     IF MID-KVDAGAR-PP-IN NOT = ALL '+'                                   
132900       IF MID-KVDAGAR-PP-IN NUMERIC                                       
133000         MOVE MFS-NUM-FAELT-RAETT TO MOD-KVDAGAR-PP-IN-ATTR               
133100       ELSE                                                               
133200         MOVE MFS-NUM-FAELT-FEL TO MOD-KVDAGAR-PP-IN-ATTR                 
133300         MOVE NEJ TO INDATA-SW                                            
133400       END-IF                                                             
133500     END-IF                                                               
133600                                                                          
133700     IF MID-KDAKDISP-DAG-IN NOT = ALL '+'                                 
133800       IF MID-KDAKDISP-DAG-IN NUMERIC AND                                 
133900         (MID-KDAKDISP-DAG-IN = 0 OR 1 OR 2)                              
134000         MOVE MFS-NUM-FAELT-RAETT TO MOD-KDAKDISP-DAG-IN-ATTR             
134100       ELSE                                                               
134200         MOVE MFS-NUM-FAELT-FEL   TO MOD-KDAKDISP-DAG-IN-ATTR             
134300         MOVE NEJ TO INDATA-SW                                            
134400       END-IF                                                             
134500     ELSE                                                                 
134600       MOVE MFS-NUM-FAELT-RAETT   TO MOD-KDAKDISP-DAG-IN-ATTR             
134700     END-IF                                                               
134800                                                                          
134900     IF MID-KDAKDISP-BULK-IN NOT = ALL '+'                                
135000       IF MID-KDAKDISP-BULK-IN NUMERIC AND                                
135100         (MID-KDAKDISP-BULK-IN = 0 OR 1 OR 2)                             
135200         MOVE MFS-NUM-FAELT-RAETT TO MOD-KDAKDISP-BULK-IN-ATTR            
135300       ELSE                                                               
135400         MOVE MFS-NUM-FAELT-FEL   TO MOD-KDAKDISP-BULK-IN-ATTR            
135500         MOVE NEJ TO INDATA-SW                                            
135600       END-IF                                                             
135700     ELSE                                                                 
135800       MOVE MFS-NUM-FAELT-RAETT   TO MOD-KDAKDISP-BULK-IN-ATTR            
135900     END-IF                                                               
136000                                                                          
136100     IF MID-FLCLEAR-BULK-IN NOT = ALL '+'                                 
136200       IF MID-FLCLEAR-BULK-IN = 'J' OR 'Y' OR 'N'                         
136300         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLCLEAR-BULK-IN-ATTR            
136400       ELSE                                                               
136500         MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLCLEAR-BULK-IN-ATTR            
136600         MOVE NEJ TO INDATA-SW                                            
136700       END-IF                                                             
136800     ELSE                                                                 
136900       MOVE MFS-ALFA-FAELT-RAETT   TO MOD-FLCLEAR-BULK-IN-ATTR            
137000     END-IF                                                               
137100                                                                          
137200     IF MID-KDSKRMET-IN NOT = ALL '+'                                     
137300       IF MID-KDSKRMET-IN NUMERIC                                         
137400         MOVE MFS-NUM-FAELT-RAETT TO MOD-KDSKRMET-IN-ATTR                 
137500       ELSE                                                               
137600         MOVE MFS-NUM-FAELT-FEL TO MOD-KDSKRMET-IN-ATTR                   
137700         MOVE NEJ TO INDATA-SW                                            
137800       END-IF                                                             
137900     END-IF                                                               
138000                                                                          
138100     IF MID-KVDAGAR-CROSS-IN NOT = ALL '+'                                
138200       IF MID-KVDAGAR-CROSS-IN NUMERIC                                    
138300         IF MID-KVDAGAR-CROSS-IN = 0 OR 1 OR 2 OR 3 OR 4                  
138400           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVDAGAR-CROSS-IN-ATTR          
138500         ELSE                                                             
138600           MOVE MFS-NUM-FAELT-FEL TO MOD-KVDAGAR-CROSS-IN-ATTR            
138700           MOVE NEJ TO INDATA-SW                                          
138800         END-IF                                                           
138900       ELSE                                                               
139000         MOVE MFS-NUM-FAELT-FEL TO MOD-KVDAGAR-CROSS-IN-ATTR              
139100         MOVE NEJ TO INDATA-SW                                            
139200       END-IF                                                             
139300     END-IF                                                               
139400                                                                          
139500     IF MID-REQXBRYT-IN NOT = ALL '+'                                     
139600       IF MID-REQXBRYT-IN NUMERIC                                         
139700         MOVE MFS-NUM-FAELT-RAETT TO MOD-REQXBRYT-IN-ATTR                 
139800       ELSE                                                               
139900         MOVE MFS-NUM-FAELT-FEL TO MOD-REQXBRYT-IN-ATTR                   
140000         MOVE NEJ TO INDATA-SW                                            
140100       END-IF                                                             
140200     END-IF                                                               
140300                                                                          
140400     IF MID-REWILSON-IN NOT = ALL '+'                                     
140500       MOVE +1                    TO DEC-KVHELTAL                         
140600       MOVE +2                    TO DEC-KVDECIMAL                        
140700       MOVE MID-REWILSON-IN       TO DEC-IDFRIDATA                        
140800       CALL WDECEDIT USING DEC-WDECAREA                                   
140900       IF DEC-KDSVAR-OK                                                   
141000         MOVE MFS-NUM-FAELT-RAETT TO MOD-REWILSON-IN-ATTR                 
141100         MOVE DEC-IDEDITDATA      TO WS-REWILSON-P                        
141200       ELSE                                                               
141300         MOVE MFS-NUM-FAELT-FEL   TO MOD-REWILSON-IN-ATTR                 
141400         MOVE NEJ TO INDATA-SW                                            
141500       END-IF                                                             
141600     END-IF                                                               
141700                                                                          
141800     IF MID-FLKNDVAL-IN NOT = ALL '+'                                     
141900       IF MID-FLKNDVAL-IN = 'J' OR 'N' OR 'Y'                             
142000         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLKNDVAL-IN-ATTR                
142100       ELSE                                                               
142200         MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLKNDVAL-IN-ATTR                
142300         MOVE NEJ                  TO INDATA-SW                           
142400       END-IF                                                             
142500     ELSE                                                                 
142600       MOVE MFS-ALFA-FAELT-RAETT   TO MOD-FLKNDVAL-IN-ATTR                
142700     END-IF                                                               
142800                                                                          
142900     IF MID-KDFAKTDC-IN NOT = ALL '+'                                     
143000       IF MID-KDFAKTDC-IN = 'A' OR 'C' OR ' '                             
143100         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDFAKTDC-IN-ATTR                
143200       ELSE                                                               
143300         MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDFAKTDC-IN-ATTR                
143400         MOVE NEJ                  TO INDATA-SW                           
143500       END-IF                                                             
143600     ELSE                                                                 
143700       MOVE MFS-ALFA-FAELT-RAETT   TO MOD-KDFAKTDC-IN-ATTR                
143800     END-IF                                                               
143900                                                                          
144000     IF MID-TID-RETOS-IN NOT = ALL '+'                                    
144100       IF MID-TID-RETOS-IN = 'N' OR 'MO' OR 'TU' OR 'WE' OR               
144200                                    'TH' OR 'FR'                          
144300         MOVE MFS-ALFA-FAELT-RAETT TO MOD-TID-RETOS-IN-ATTR               
144400       ELSE                                                               
144500         MOVE MFS-ALFA-FAELT-FEL TO MOD-TID-RETOS-IN-ATTR                 
144600         MOVE NEJ                  TO INDATA-SW                           
144700       END-IF                                                             
144800     ELSE                                                                 
144900       MOVE MFS-ALFA-FAELT-RAETT   TO MOD-TID-RETOS-IN-ATTR               
145000     END-IF                                                               
145100                                                                          
145200     IF MID-TID-RET98-IN NOT = ALL '+'                                    
145300       IF MID-TID-RET98-IN = 'N' OR 'MO' OR 'TU' OR 'WE' OR               
145400                                    'TH' OR 'FR'                          
145500         MOVE MFS-ALFA-FAELT-RAETT TO MOD-TID-RET98-IN-ATTR               
145600       ELSE                                                               
145700         MOVE MFS-ALFA-FAELT-FEL TO MOD-TID-RET98-IN-ATTR                 
145800         MOVE NEJ                  TO INDATA-SW                           
145900       END-IF                                                             
146000       MOVE MFS-ALFA-FAELT-RAETT   TO MOD-TID-RET98-IN-ATTR               
146100     END-IF                                                               
146200                                                                          
146300     IF MID-PRARTSTD-SKRLO98-IN NOT = ALL '+'                             
146400       IF MID-PRARTSTD-SKRLO98-IN NUMERIC                                 
146500         MOVE MFS-NUM-FAELT-RAETT TO MOD-PRARTSTD-SKRLO98-IN-ATTR         
146600       ELSE                                                               
146700         MOVE MFS-NUM-FAELT-FEL   TO MOD-PRARTSTD-SKRLO98-IN-ATTR         
146800         MOVE NEJ TO INDATA-SW                                            
146900       END-IF                                                             
147000     ELSE                                                                 
147100       MOVE MFS-NUM-FAELT-RAETT   TO MOD-PRARTSTD-SKRLO98-IN-ATTR         
147200     END-IF                                                               
147300                                                                          
147400     IF  MID-FLTRLO88-MAN-IN NOT = ALL '+'                                
147500       IF DCS-SDC OR DCS-CDC                                              
147600         IF MID-FLTRLO88-MAN-IN = 'N' OR 'Y'                              
147700           MOVE MFS-ALFA-FAELT-RAETT  TO MOD-FLTRLO88-MAN-IN-ATTR         
147800         ELSE                                                             
147900           MOVE MFS-ALFA-FAELT-FEL    TO MOD-FLTRLO88-MAN-IN-ATTR         
148000           MOVE NEJ                   TO INDATA-SW                        
148100         END-IF                                                           
148200       ELSE                                                               
148300         MOVE MFS-ALFA-FAELT-FEL       TO MOD-FLTRLO88-MAN-IN-ATTR        
148400         MOVE 'ONLY APPLY FOR SDC/LDC' TO MOD-TEMFSFEL                    
148500         MOVE NEJ                      TO INDATA-SW                       
148600       END-IF                                                             
148700     ELSE                                                                 
148800       MOVE MFS-ALFA-FAELT-RAETT      TO MOD-FLTRLO88-MAN-IN-ATTR         
148900     END-IF                                                               
149000                                                                          
149100     IF  MID-FLTRLO88-TIS-IN NOT = ALL '+'                                
149200       IF DCS-SDC OR DCS-CDC                                              
149300         IF MID-FLTRLO88-TIS-IN = 'N' OR 'Y'                              
149400           MOVE MFS-ALFA-FAELT-RAETT  TO MOD-FLTRLO88-TIS-IN-ATTR         
149500         ELSE                                                             
149600           MOVE MFS-ALFA-FAELT-FEL    TO MOD-FLTRLO88-TIS-IN-ATTR         
149700           MOVE NEJ                   TO INDATA-SW                        
149800         END-IF                                                           
149900       ELSE                                                               
150000         MOVE MFS-ALFA-FAELT-FEL       TO MOD-FLTRLO88-TIS-IN-ATTR        
150100         MOVE 'ONLY APPLY FOR SDC/LDC' TO MOD-TEMFSFEL                    
150200         MOVE NEJ                      TO INDATA-SW                       
150300       END-IF                                                             
150400     ELSE                                                                 
150500       MOVE MFS-ALFA-FAELT-RAETT      TO MOD-FLTRLO88-TIS-IN-ATTR         
150600     END-IF                                                               
150700                                                                          
150800     IF  MID-FLTRLO88-ONS-IN NOT = ALL '+'                                
150900       IF DCS-SDC OR DCS-CDC                                              
151000         IF MID-FLTRLO88-ONS-IN = 'N' OR 'Y'                              
151100           MOVE MFS-ALFA-FAELT-RAETT  TO MOD-FLTRLO88-ONS-IN-ATTR         
151200         ELSE                                                             
151300           MOVE MFS-ALFA-FAELT-FEL    TO MOD-FLTRLO88-ONS-IN-ATTR         
151400           MOVE NEJ                   TO INDATA-SW                        
151500         END-IF                                                           
151600       ELSE                                                               
151700         MOVE MFS-ALFA-FAELT-FEL       TO MOD-FLTRLO88-ONS-IN-ATTR        
151800         MOVE 'ONLY APPLY FOR SDC/LDC' TO MOD-TEMFSFEL                    
151900         MOVE NEJ                      TO INDATA-SW                       
152000       END-IF                                                             
152100     ELSE                                                                 
152200       MOVE MFS-ALFA-FAELT-RAETT      TO MOD-FLTRLO88-ONS-IN-ATTR         
152300     END-IF                                                               
152400                                                                          
152500     IF  MID-FLTRLO88-TOR-IN NOT = ALL '+'                                
152600       IF DCS-SDC OR DCS-CDC                                              
152700         IF MID-FLTRLO88-TOR-IN = 'N' OR 'Y'                              
152800           MOVE MFS-ALFA-FAELT-RAETT  TO MOD-FLTRLO88-TOR-IN-ATTR         
152900         ELSE                                                             
153000           MOVE MFS-ALFA-FAELT-FEL    TO MOD-FLTRLO88-TOR-IN-ATTR         
153100           MOVE NEJ                   TO INDATA-SW                        
153200         END-IF                                                           
153300       ELSE                                                               
153400         MOVE MFS-ALFA-FAELT-FEL       TO MOD-FLTRLO88-TOR-IN-ATTR        
153500         MOVE 'ONLY APPLY FOR SDC/LDC' TO MOD-TEMFSFEL                    
153600         MOVE NEJ                      TO INDATA-SW                       
153700       END-IF                                                             
153800     ELSE                                                                 
153900       MOVE MFS-ALFA-FAELT-RAETT      TO MOD-FLTRLO88-TOR-IN-ATTR         
154000     END-IF                                                               
154100                                                                          
154200     IF  MID-FLTRLO88-FRE-IN NOT = ALL '+'                                
154300       IF DCS-SDC OR DCS-CDC                                              
154400         IF MID-FLTRLO88-FRE-IN = 'N' OR 'Y'                              
154500           MOVE MFS-ALFA-FAELT-RAETT  TO MOD-FLTRLO88-FRE-IN-ATTR         
154600         ELSE                                                             
154700           MOVE MFS-ALFA-FAELT-FEL    TO MOD-FLTRLO88-FRE-IN-ATTR         
154800           MOVE NEJ                   TO INDATA-SW                        
154900         END-IF                                                           
155000       ELSE                                                               
155100         MOVE MFS-ALFA-FAELT-FEL       TO MOD-FLTRLO88-FRE-IN-ATTR        
155200         MOVE 'ONLY APPLY FOR SDC/LDC' TO MOD-TEMFSFEL                    
155300         MOVE NEJ                      TO INDATA-SW                       
155400       END-IF                                                             
155500     ELSE                                                                 
155600       MOVE MFS-ALFA-FAELT-RAETT      TO MOD-FLTRLO88-FRE-IN-ATTR         
155700     END-IF                                                               
155800                                                                          
155900     IF MID-IDDC-REF-IN NOT = ALL '+'                                     
156000       MOVE MID-IDDC-REF-IN      TO REF-WS-IDDC                           
156100       MOVE MSGI-IDDC-KEY        TO WS-IDDC                               
156200       IF MID-IDDC-REF-IN = W-IDDC                                        
156300         MOVE MFS-ALFA-FAELT-FEL TO MOD-IDDC-REF-IN-ATTR                  
156400         MOVE NEJ                  TO INDATA-SW                           
156500         MOVE 'REF FROM DC SAME AS DC'    TO MOD-TEMFSINF                 
156600       ELSE                                                               
156700         MOVE MID-IDDC-REF-IN      TO W-IDDC                              
156800         PERFORM IMS-GU-WDB601                                            
156900         IF SEGMENT-FINNS                                                 
156920           MOVE MSGI-IDDC-KEY            TO W-IDDC                        
156930           MOVE '4408'                   TO W-IDTRANS-B6                  
156940           MOVE MID-IDDC-REF-IN          TO W-IDDC-REF-B6                 
156950           PERFORM IMS-GU-WDB615                                          
156960           IF SEGMENT-FINNS                                               
156961             IF MID-IDDC-REF-IN      = WC-CDC-SE                          
156962*------------ DISTRICT DETAILS FOR CDC ARE STORED ON WDB601               
156963               MOVE MSGI-IDDC-KEY        TO W-IDDC                        
156964               PERFORM IMS-GU-WDB601                                      
156965               IF  SEGMENT-FINNS                                          
156966               AND DCS-IDDISTR-REFILL > ZERO                              
156967                 MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDDC-REF-IN-ATTR        
156969               ELSE                                                       
156970                 MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDDC-REF-IN-ATTR        
156971                 MOVE NEJ                  TO INDATA-SW                   
156972                 MOVE 'REFILL FLOW NOT AVAILABLE'                         
156973                                           TO MOD-TEMFSINF                
156974               END-IF                                                     
156975             ELSE                                                         
156976*------------ DISTRICT DETAILS FOR OTHER DCS ARE STORED ON WDB616         
156980               MOVE MID-IDDC-REF-IN      TO W-IDDC-REF                    
156997               PERFORM IMS-GU-WDB616                                      
156998               IF  SEGMENT-FINNS                                          
156999               AND REF-IDDISTR-REFILL > ZERO                              
157000                 MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDDC-REF-IN-ATTR        
157001               ELSE                                                       
157002                 MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDDC-REF-IN-ATTR        
157003                 MOVE NEJ                  TO INDATA-SW                   
157004                 MOVE 'REFILL FLOW NOT AVAILABLE'                         
157005                                           TO MOD-TEMFSINF                
157006               END-IF                                                     
157007             END-IF                                                       
157023           ELSE                                                           
157024             MOVE MFS-ALFA-FAELT-FEL     TO MOD-IDDC-REF-IN-ATTR          
157025             MOVE NEJ                    TO INDATA-SW                     
157026             MOVE 'REFILL FLOW NOT AVAILABLE'                             
157027                                         TO MOD-TEMFSINF                  
157030           END-IF                                                         
157100         ELSE                                                             
157200           MOVE MFS-ALFA-FAELT-FEL       TO MOD-IDDC-REF-IN-ATTR          
157300           MOVE NEJ                      TO INDATA-SW                     
157400           MOVE 'REF FROM DC NOT ACTIVE' TO MOD-TEMFSINF                  
157500         END-IF                                                           
157600       END-IF                                                             
157700     ELSE                                                                 
157800       MOVE MFS-ALFA-FAELT-RAETT   TO MOD-IDDC-REF-IN-ATTR                
157900     END-IF                                                               
158000     .                                                                    
158100     EJECT                                                                
158200 H-UPPDATERA SECTION.                                                     
158300                                                                          
158400     MOVE MSGI-IDDC-KEY TO W-IDDC                                         
158500     PERFORM IMS-GHU-WDB601                                               
158600                                                                          
158700     IF MID-FLDCRET-IN NOT = ALL '+'                                      
158800       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLDCRET-IN-ATTR                  
158900       MOVE 'FLDCRET'         TO DCSL-BETEXT-ITEM                         
159000       MOVE DCS-FLDCRET       TO DCSL-BETEXT-OLDDATA                      
159100       IF MID-FLDCRET-IN = YES                                            
159200          MOVE JA             TO DCS-FLDCRET                              
159300       ELSE                                                               
159400          MOVE MID-FLDCRET-IN TO DCS-FLDCRET                              
159500       END-IF                                                             
159600       MOVE MID-FLDCRET-IN    TO DCSL-BETEXT-NEWDATA                      
159700       ADD +1                 TO FIL-IDSEKVNR                             
159800       PERFORM IMS-ISRT-WDR601                                            
159900     END-IF                                                               
160000                                                                          
160100     IF MID-FLINLHIST-IN NOT = ALL '+'                                    
160200       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLINLHIST-IN-ATTR                
160300       MOVE 'FLINLHIST'         TO DCSL-BETEXT-ITEM                       
160400       MOVE DCS-FLINLHIST       TO DCSL-BETEXT-OLDDATA                    
160500       IF MID-FLINLHIST-IN = YES                                          
160600          MOVE JA               TO DCS-FLINLHIST                          
160700       ELSE                                                               
160800          MOVE MID-FLINLHIST-IN TO DCS-FLINLHIST                          
160900       END-IF                                                             
161000       MOVE MID-FLINLHIST-IN    TO DCSL-BETEXT-NEWDATA                    
161100       ADD +1                   TO FIL-IDSEKVNR                           
161200       PERFORM IMS-ISRT-WDR601                                            
161300     END-IF                                                               
161400                                                                          
161500     IF MID-FLFSEDEL-IN NOT = ALL '+'                                     
161600       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLFSEDEL-IN-ATTR                 
161700       MOVE 'FLFSEDEL'         TO DCSL-BETEXT-ITEM                        
161800       MOVE DCS-FLFSEDEL       TO DCSL-BETEXT-OLDDATA                     
161900       IF MID-FLFSEDEL-IN = YES                                           
162000          MOVE JA              TO DCS-FLFSEDEL                            
162100       ELSE                                                               
162200          MOVE MID-FLFSEDEL-IN TO DCS-FLFSEDEL                            
162300       END-IF                                                             
162400       MOVE MID-FLFSEDEL-IN    TO DCSL-BETEXT-NEWDATA                     
162500       ADD +1                  TO FIL-IDSEKVNR                            
162600       PERFORM IMS-ISRT-WDR601                                            
162700     END-IF                                                               
162800                                                                          
162900     IF MID-FLINLREP-IN NOT = ALL '+'                                     
163000       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLINLREP-IN-ATTR                 
163100       MOVE 'FLINLREP'         TO DCSL-BETEXT-ITEM                        
163200       MOVE DCS-FLINLREP       TO DCSL-BETEXT-OLDDATA                     
163300       IF MID-FLINLREP-IN = YES                                           
163400          MOVE JA              TO DCS-FLINLREP                            
163500       ELSE                                                               
163600          MOVE MID-FLINLREP-IN TO DCS-FLINLREP                            
163700       END-IF                                                             
163800        MOVE MID-FLINLREP-IN   TO DCSL-BETEXT-NEWDATA                     
163900       ADD +1                  TO FIL-IDSEKVNR                            
164000       PERFORM IMS-ISRT-WDR601                                            
164100     END-IF                                                               
164200                                                                          
164300     IF MID-FLPRISSPR-IN NOT = ALL '+'                                    
164400       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLPRISSPR-IN-ATTR                
164500       MOVE 'FLPRISSPR'         TO DCSL-BETEXT-ITEM                       
164600       MOVE DCS-FLPRISSPR       TO DCSL-BETEXT-OLDDATA                    
164700       IF MID-FLPRISSPR-IN = YES                                          
164800          MOVE JA               TO DCS-FLPRISSPR                          
164900       ELSE                                                               
165000          MOVE MID-FLPRISSPR-IN TO DCS-FLPRISSPR                          
165100       END-IF                                                             
165200       MOVE MID-FLPRISSPR-IN    TO DCSL-BETEXT-NEWDATA                    
165300       ADD +1                   TO FIL-IDSEKVNR                           
165400       PERFORM IMS-ISRT-WDR601                                            
165500     END-IF                                                               
165600                                                                          
165700     IF MID-FLBINNUT-IN NOT = ALL '+'                                     
165800       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLBINNUT-IN-ATTR                 
165900       MOVE 'FLBINNUT'         TO DCSL-BETEXT-ITEM                        
166000       MOVE DCS-FLBINNUT       TO DCSL-BETEXT-OLDDATA                     
166100       IF MID-FLBINNUT-IN = YES                                           
166200          MOVE JA              TO DCS-FLBINNUT                            
166300       ELSE                                                               
166400          MOVE MID-FLBINNUT-IN TO DCS-FLBINNUT                            
166500       END-IF                                                             
166600       MOVE MID-FLBINNUT-IN    TO DCSL-BETEXT-NEWDATA                     
166700       ADD +1                  TO FIL-IDSEKVNR                            
166800       PERFORM IMS-ISRT-WDR601                                            
166900     END-IF                                                               
167000                                                                          
167100     IF MID-FLSAMPAK-IN NOT = ALL '+'                                     
167200       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLSAMPAK-IN-ATTR                 
167300       MOVE 'FLSAMPAK'        TO DCSL-BETEXT-ITEM                         
167400       MOVE DCS-FLSAMPAK      TO DCSL-BETEXT-OLDDATA                      
167500       IF MID-FLSAMPAK-IN = YES                                           
167600          MOVE JA              TO DCS-FLSAMPAK                            
167700       ELSE                                                               
167800          MOVE MID-FLSAMPAK-IN TO DCS-FLSAMPAK                            
167900       END-IF                                                             
168000       MOVE MID-FLSAMPAK-IN   TO DCSL-BETEXT-NEWDATA                      
168100       ADD +1                 TO FIL-IDSEKVNR                             
168200       PERFORM IMS-ISRT-WDR601                                            
168300     END-IF                                                               
168400                                                                          
168500     IF MID-FLSEASBER-IN NOT = ALL '+'                                    
168600       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLSEASBER-IN-ATTR                
168700       MOVE 'FLSEASBER'         TO DCSL-BETEXT-ITEM                       
168800       MOVE DCS-FLSEASBER       TO DCSL-BETEXT-OLDDATA                    
168900       IF MID-FLSEASBER-IN = YES                                          
169000          MOVE JA               TO DCS-FLSEASBER                          
169100       ELSE                                                               
169200          MOVE MID-FLSEASBER-IN TO DCS-FLSEASBER                          
169300       END-IF                                                             
169400       MOVE MID-FLSEASBER-IN    TO DCSL-BETEXT-NEWDATA                    
169500       ADD +1                   TO FIL-IDSEKVNR                           
169600       PERFORM IMS-ISRT-WDR601                                            
169700     END-IF                                                               
169800                                                                          
169900     IF MID-FLTYP6JU-IN NOT = ALL '+'                                     
170000       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLTYP6JU-IN-ATTR                 
170100       MOVE 'FLTYP6JU'         TO DCSL-BETEXT-ITEM                        
170200       MOVE DCS-FLTYP6JU       TO DCSL-BETEXT-OLDDATA                     
170300       IF MID-FLTYP6JU-IN = YES                                           
170400          MOVE JA              TO DCS-FLTYP6JU                            
170500       ELSE                                                               
170600          MOVE MID-FLTYP6JU-IN TO DCS-FLTYP6JU                            
170700       END-IF                                                             
170800       MOVE MID-FLTYP6JU-IN    TO DCSL-BETEXT-NEWDATA                     
170900       ADD +1                  TO FIL-IDSEKVNR                            
171000       PERFORM IMS-ISRT-WDR601                                            
171100     END-IF                                                               
171200                                                                          
171300     IF MID-FLRSI-IN NOT = ALL '+'                                        
171400       MOVE MFS-ADD-LYS-UPP-FAELT    TO MOD-FLRSI-IN-ATTR                 
171500       MOVE 'FLRSI'                  TO DCSL-BETEXT-ITEM                  
171600       MOVE DCS-FLRSI                TO DCSL-BETEXT-OLDDATA               
171700       IF MID-FLRSI-IN = YES                                              
171800          MOVE JA                    TO DCS-FLRSI                         
171900       ELSE                                                               
172000          MOVE MID-FLRSI-IN          TO DCS-FLRSI                         
172100       END-IF                                                             
172200       MOVE MID-FLRSI-IN             TO DCSL-BETEXT-NEWDATA               
172300       ADD +1                        TO FIL-IDSEKVNR                      
172400       PERFORM IMS-ISRT-WDR601                                            
172500     END-IF                                                               
172600                                                                          
172700     IF MID-KDPORDL-IN NOT = ALL '+'                                      
172800       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDPORDL-IN-ATTR                  
172900       MOVE 'KDPORDL'         TO DCSL-BETEXT-ITEM                         
173000       MOVE DCS-KDPORDL       TO DCSL-BETEXT-OLDDATA                      
173100       IF MID-KDPORDL-IN = YES                                            
173200          MOVE JA             TO DCS-KDPORDL                              
173300       ELSE                                                               
173400          MOVE MID-KDPORDL-IN TO DCS-KDPORDL                              
173500       END-IF                                                             
173600       MOVE MID-KDPORDL-IN    TO DCSL-BETEXT-NEWDATA                      
173700       ADD +1                 TO FIL-IDSEKVNR                             
173800       PERFORM IMS-ISRT-WDR601                                            
173900     END-IF                                                               
174000                                                                          
174100     IF MID-IDLISTNR-IN NOT = ALL '+'                                     
174200       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDLISTNR-IN-ATTR                 
174300       MOVE 'IDLISTNR'        TO DCSL-BETEXT-ITEM                         
174400       MOVE DCS-IDLISTNR      TO DCSL-BETEXT-OLDDATA                      
174500       MOVE MID-IDLISTNR-IN   TO DCS-IDLISTNR                             
174600                                 DCSL-BETEXT-NEWDATA                      
174700       ADD +1                 TO FIL-IDSEKVNR                             
174800       PERFORM IMS-ISRT-WDR601                                            
174900     END-IF                                                               
175000                                                                          
175100     IF MID-FLEXCP1-PRIO-IN NOT = ALL '+'                                 
175200       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLEXCP1-PRIO-IN-ATTR             
175300       MOVE 'FLEXCP1-PRIO'         TO DCSL-BETEXT-ITEM                    
175400       MOVE DCS-FLEXCP1-PRIO       TO DCSL-BETEXT-OLDDATA                 
175500       IF MID-FLEXCP1-PRIO-IN = YES                                       
175600          MOVE JA                  TO DCS-FLEXCP1-PRIO                    
175700       ELSE                                                               
175800          MOVE MID-FLEXCP1-PRIO-IN TO DCS-FLEXCP1-PRIO                    
175900       END-IF                                                             
176000       MOVE MID-FLEXCP1-PRIO-IN    TO DCSL-BETEXT-NEWDATA                 
176100       ADD +1                      TO FIL-IDSEKVNR                        
176200       PERFORM IMS-ISRT-WDR601                                            
176300     END-IF                                                               
176400                                                                          
176500     IF MID-FLEXCP1-REFBEO-IN NOT = ALL '+'                               
176600       MOVE MFS-ADD-LYS-UPP-FAELT    TO MOD-FLEXCP1-REFBEO-IN-ATTR        
176700       MOVE 'FLEXCP1-REFBEO'         TO DCSL-BETEXT-ITEM                  
176800       MOVE DCS-FLEXCP1-REFBEO       TO DCSL-BETEXT-OLDDATA               
177200       MOVE MID-FLEXCP1-REFBEO-IN    TO DCS-FLEXCP1-REFBEO                
177400       MOVE MID-FLEXCP1-REFBEO-IN    TO DCSL-BETEXT-NEWDATA               
177500       ADD +1                        TO FIL-IDSEKVNR                      
177600       PERFORM IMS-ISRT-WDR601                                            
177700     END-IF                                                               
177800                                                                          
177900     IF MID-FLEXCP2-REFBEO-IN NOT = ALL '+'                               
178000       MOVE MFS-ADD-LYS-UPP-FAELT    TO MOD-FLEXCP2-REFBEO-IN-ATTR        
178100       MOVE 'FLEXCP2-REFBEO'         TO DCSL-BETEXT-ITEM                  
178200       MOVE DCS-FLEXCP2-REFBEO       TO DCSL-BETEXT-OLDDATA               
178600       MOVE MID-FLEXCP2-REFBEO-IN    TO DCS-FLEXCP2-REFBEO                
178800       MOVE MID-FLEXCP2-REFBEO-IN    TO DCSL-BETEXT-NEWDATA               
178900       ADD +1                        TO FIL-IDSEKVNR                      
179000       PERFORM IMS-ISRT-WDR601                                            
179100     END-IF                                                               
179200                                                                          
179300     IF MID-FLEXCP3-REFBEO-IN NOT = ALL '+'                               
179400       MOVE MFS-ADD-LYS-UPP-FAELT    TO MOD-FLEXCP3-REFBEO-IN-ATTR        
179500       MOVE 'FLEXCP3-REFBEO'         TO DCSL-BETEXT-ITEM                  
179600       MOVE DCS-FLEXCP3-REFBEO       TO DCSL-BETEXT-OLDDATA               
179700       IF MID-FLEXCP3-REFBEO-IN = YES                                     
179800          MOVE JA                    TO DCS-FLEXCP3-REFBEO                
179900       ELSE                                                               
180000          MOVE MID-FLEXCP3-REFBEO-IN TO DCS-FLEXCP3-REFBEO                
180100       END-IF                                                             
180200       MOVE MID-FLEXCP3-REFBEO-IN    TO DCSL-BETEXT-NEWDATA               
180300       ADD +1                        TO FIL-IDSEKVNR                      
180400       PERFORM IMS-ISRT-WDR601                                            
180500     END-IF                                                               
180600                                                                          
180700     IF MID-FLEXCP4-REFBEO-IN NOT = ALL '+'                               
180800       MOVE MFS-ADD-LYS-UPP-FAELT    TO MOD-FLEXCP4-REFBEO-IN-ATTR        
180900       MOVE 'FLEXCP4-REFBEO'         TO DCSL-BETEXT-ITEM                  
181000       MOVE DCS-FLEXCP4-REFBEO       TO DCSL-BETEXT-OLDDATA               
181400       MOVE MID-FLEXCP4-REFBEO-IN    TO DCS-FLEXCP4-REFBEO                
181600       MOVE MID-FLEXCP4-REFBEO-IN    TO DCSL-BETEXT-NEWDATA               
181700       ADD +1                        TO FIL-IDSEKVNR                      
181800       PERFORM IMS-ISRT-WDR601                                            
181900     END-IF                                                               
182000                                                                          
182100     IF MID-FLEXCP1-REFBER-IN NOT = ALL '+'                               
182200       MOVE MFS-ADD-LYS-UPP-FAELT    TO MOD-FLEXCP1-REFBER-IN-ATTR        
182300       MOVE 'FLEXCP1-REFBER'         TO DCSL-BETEXT-ITEM                  
182400       MOVE DCS-FLEXCP1-REFBER       TO DCSL-BETEXT-OLDDATA               
182500       IF MID-FLEXCP1-REFBER-IN = YES                                     
182600          MOVE JA                    TO DCS-FLEXCP1-REFBER                
182700       ELSE                                                               
182800          MOVE MID-FLEXCP1-REFBER-IN TO DCS-FLEXCP1-REFBER                
182900       END-IF                                                             
183000       MOVE MID-FLEXCP1-REFBER-IN    TO DCSL-BETEXT-NEWDATA               
183100       ADD +1                        TO FIL-IDSEKVNR                      
183200       PERFORM IMS-ISRT-WDR601                                            
183300     END-IF                                                               
183400                                                                          
183500     IF MID-FLEXCP2-REFBER-IN NOT = ALL '+'                               
183600       MOVE MFS-ADD-LYS-UPP-FAELT    TO MOD-FLEXCP2-REFBER-IN-ATTR        
183700       MOVE 'FLEXCP2-REFBER'         TO DCSL-BETEXT-ITEM                  
183800       MOVE DCS-FLEXCP2-REFBER       TO DCSL-BETEXT-OLDDATA               
183900       IF MID-FLEXCP2-REFBER-IN = YES                                     
184000          MOVE JA                    TO DCS-FLEXCP2-REFBER                
184100       ELSE                                                               
184200          MOVE MID-FLEXCP2-REFBER-IN TO DCS-FLEXCP2-REFBER                
184300       END-IF                                                             
184400       MOVE MID-FLEXCP2-REFBER-IN    TO DCSL-BETEXT-NEWDATA               
184500       ADD +1                        TO FIL-IDSEKVNR                      
184600       PERFORM IMS-ISRT-WDR601                                            
184700     END-IF                                                               
184800                                                                          
184900     IF MID-FLOVRLAGBER-IN NOT = ALL '+'                                  
185000       MOVE MFS-ADD-LYS-UPP-FAELT    TO MOD-FLOVRLAGBER-IN-ATTR           
185100       MOVE 'FLOVRLAGBER'            TO DCSL-BETEXT-ITEM                  
185200       MOVE DCS-FLOVRLAGBER          TO DCSL-BETEXT-OLDDATA               
185300       IF MID-FLOVRLAGBER-IN = JA OR YES                                  
185400          MOVE YES                   TO DCS-FLOVRLAGBER                   
185500       ELSE                                                               
185600          MOVE MID-FLOVRLAGBER-IN    TO DCS-FLOVRLAGBER                   
185700       END-IF                                                             
185800       MOVE MID-FLOVRLAGBER-IN       TO DCSL-BETEXT-NEWDATA               
185900       ADD +1                        TO FIL-IDSEKVNR                      
186000       PERFORM IMS-ISRT-WDR601                                            
186100     END-IF                                                               
186200                                                                          
186300     IF MID-IDPRTLST-INVA-IN NOT = ALL '+'                                
186400       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDPRTLST-INVA-IN-ATTR            
186500       MOVE 'IDPRTLST-INVA' TO DCSL-BETEXT-ITEM                           
186600       MOVE DCS-IDPRTLST-INVA    TO DCSL-BETEXT-OLDDATA                   
186700       MOVE MID-IDPRTLST-INVA-IN TO DCS-IDPRTLST-INVA                     
186800                                 DCSL-BETEXT-NEWDATA                      
186900       ADD +1                 TO FIL-IDSEKVNR                             
187000       PERFORM IMS-ISRT-WDR601                                            
187100     END-IF                                                               
187200                                                                          
187200                                                                          
187300     IF MID-FLARTADD-IN NOT = ALL '+'                                     
187400       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLARTADD-IN-ATTR                 
187500       MOVE 'FLARTADD'      TO DCSL-BETEXT-ITEM                           
187600       MOVE DCS-FLARTADD         TO DCSL-BETEXT-OLDDATA                   
187700       MOVE MID-FLARTADD-IN      TO DCS-FLARTADD                          
187800                                 DCSL-BETEXT-NEWDATA                      
187900       ADD +1                 TO FIL-IDSEKVNR                             
188000       PERFORM IMS-ISRT-WDR601                                            
188100     END-IF                                                               
188200                                                                          
187300     IF MID-KVINVAUT-IN NOT = ALL '+'                                     
187400       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVINVAUT-IN-ATTR                 
187500       MOVE 'KVINVAUT'      TO DCSL-BETEXT-ITEM                           
187600       MOVE DCS-KVINVAUT         TO DCSL-BETEXT-OLDDATA                   
187700       MOVE MID-KVINVAUT-IN      TO DCS-KVINVAUT                          
187800                                 DCSL-BETEXT-NEWDATA                      
187900       ADD +1                 TO FIL-IDSEKVNR                             
188000       PERFORM IMS-ISRT-WDR601                                            
188100     END-IF                                                               
188200                                                                          
188300     IF MID-SUINVGRANS-IN NOT = ALL '+'                                   
188400       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-SUINVGRANS-IN-ATTR               
188500       MOVE 'SUINVGRANS'         TO DCSL-BETEXT-ITEM                      
188600       MOVE DCS-SUINVGRANS       TO DCSL-BETEXT-OLDDATA                   
188700       MOVE MID-SUINVGRANS-IN    TO DCS-SUINVGRANS                        
188800                                    DCSL-BETEXT-NEWDATA                   
188900       ADD +1                    TO FIL-IDSEKVNR                          
189000       PERFORM IMS-ISRT-WDR601                                            
189100     END-IF                                                               
189200                                                                          
189300     IF MID-IDPRTLST-INVAB-IN NOT = ALL '+'                               
189400       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDPRTLST-INVAB-IN-ATTR           
189500       MOVE 'IDPRTLST-INVAB' TO DCSL-BETEXT-ITEM                          
189600       MOVE DCS-IDPRTLST-INVAB   TO DCSL-BETEXT-OLDDATA                   
189700       MOVE MID-IDPRTLST-INVAB-IN TO DCS-IDPRTLST-INVAB                   
189800                                 DCSL-BETEXT-NEWDATA                      
189900       ADD +1                 TO FIL-IDSEKVNR                             
190000       PERFORM IMS-ISRT-WDR601                                            
190100     END-IF                                                               
190200                                                                          
190300     IF MID-IDPRTLST-INL-IN NOT = ALL '+'                                 
190400       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDPRTLST-INL-IN-ATTR             
190500       MOVE 'IDPRTLST-INL' TO DCSL-BETEXT-ITEM                            
190600       MOVE DCS-IDPRTLST-INL      TO DCSL-BETEXT-OLDDATA                  
190700       MOVE MID-IDPRTLST-INL-IN TO DCS-IDPRTLST-INL                       
190800                                 DCSL-BETEXT-NEWDATA                      
190900       ADD +1                 TO FIL-IDSEKVNR                             
191000       PERFORM IMS-ISRT-WDR601                                            
191100     END-IF                                                               
191200                                                                          
191300     IF MID-IDPRTLST-INLA-IN NOT = ALL '+'                                
191400       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDPRTLST-INLA-IN-ATTR            
191500       MOVE 'IDPRTLST-INLA' TO DCSL-BETEXT-ITEM                           
191600       MOVE DCS-IDPRTLST-INLA   TO DCSL-BETEXT-OLDDATA                    
191700       MOVE MID-IDPRTLST-INLA-IN TO DCS-IDPRTLST-INLA                     
191800                                 DCSL-BETEXT-NEWDATA                      
191900       ADD +1                 TO FIL-IDSEKVNR                             
192000       PERFORM IMS-ISRT-WDR601                                            
192100     END-IF                                                               
192200                                                                          
192300     IF MID-KDDCSTYR-BUY-IN NOT = ALL '+'                                 
192400       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDDCSTYR-BUY-IN-ATTR             
192500       MOVE 'KDDCSTYR-BUY' TO DCSL-BETEXT-ITEM                            
192600       MOVE DCS-KDDCSTYR-BUY     TO DCSL-BETEXT-OLDDATA                   
192700       MOVE MID-KDDCSTYR-BUY-IN TO DCS-KDDCSTYR-BUY                       
192800                                 DCSL-BETEXT-NEWDATA                      
192900       ADD +1                 TO FIL-IDSEKVNR                             
193000       PERFORM IMS-ISRT-WDR601                                            
193100     END-IF                                                               
193200                                                                          
193300     IF MID-KDDCSTYR-KUND-IN NOT = ALL '+'                                
193400       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDDCSTYR-KUND-IN-ATTR            
193500       MOVE 'KDDCSTYR-KUND' TO DCSL-BETEXT-ITEM                           
193600       MOVE DCS-KDDCSTYR-KUND    TO DCSL-BETEXT-OLDDATA                   
193700       MOVE MID-KDDCSTYR-KUND-IN TO DCS-KDDCSTYR-KUND                     
193800                                 DCSL-BETEXT-NEWDATA                      
193900       ADD +1                 TO FIL-IDSEKVNR                             
194000       PERFORM IMS-ISRT-WDR601                                            
194100     END-IF                                                               
194200                                                                          
194300     IF MID-KDDCSTYR-REFTAB-IN NOT = ALL '+'                              
194400       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDDCSTYR-REFTAB-IN-ATTR          
194500       MOVE 'KDDCSTYR-REFTAB' TO DCSL-BETEXT-ITEM                         
194600       MOVE DCS-KDDCSTYR-REFTAB  TO DCSL-BETEXT-OLDDATA                   
194700       MOVE MID-KDDCSTYR-REFTAB-IN TO DCS-KDDCSTYR-REFTAB                 
194800                                 DCSL-BETEXT-NEWDATA                      
194900       ADD +1                 TO FIL-IDSEKVNR                             
195000       PERFORM IMS-ISRT-WDR601                                            
195100     END-IF                                                               
195200                                                                          
195300     IF MID-KVDAGAR-POKS-IN NOT = ALL '+'                                 
195400       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVDAGAR-POKS-IN-ATTR             
195500       MOVE 'KVDAGAR-POKS'        TO DCSL-BETEXT-ITEM                     
195600       MOVE DCS-KVDAGAR-POKS      TO DCSL-BETEXT-OLDDATA                  
195700       MOVE MID-KVDAGAR-POKS-IN   TO DCS-KVDAGAR-POKS                     
195800                                     DCSL-BETEXT-NEWDATA                  
195900       ADD +1                     TO FIL-IDSEKVNR                         
196000       PERFORM IMS-ISRT-WDR601                                            
196100     END-IF                                                               
196200                                                                          
196300     IF MID-KVDAGAR-PP-IN NOT = ALL '+'                                   
196400       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVDAGAR-PP-IN-ATTR               
196500       MOVE 'KVDAGAR-PP'          TO DCSL-BETEXT-ITEM                     
196600       MOVE DCS-KVDAGAR-PP        TO DCSL-BETEXT-OLDDATA                  
196700       MOVE MID-KVDAGAR-PP-IN     TO DCS-KVDAGAR-PP                       
196800                                     DCSL-BETEXT-NEWDATA                  
196900       ADD +1                     TO FIL-IDSEKVNR                         
197000       PERFORM IMS-ISRT-WDR601                                            
197100     END-IF                                                               
197200                                                                          
197300     IF MID-KDAKDISP-DAG-IN NOT = ALL '+'                                 
197400       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDAKDISP-DAG-IN-ATTR             
197500       MOVE 'KDAKDISP-DAG'        TO DCSL-BETEXT-ITEM                     
197600       MOVE DCS-KDAKDISP-DAG      TO DCSL-BETEXT-OLDDATA                  
197700       MOVE MID-KDAKDISP-DAG-IN   TO DCS-KDAKDISP-DAG                     
197800                                     DCSL-BETEXT-NEWDATA                  
197900       ADD +1                     TO FIL-IDSEKVNR                         
198000       PERFORM IMS-ISRT-WDR601                                            
198100     END-IF                                                               
198200                                                                          
198300     IF MID-KDAKDISP-BULK-IN NOT = ALL '+'                                
198400       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDAKDISP-BULK-IN-ATTR            
198500       MOVE 'KDAKDISP-BULK'       TO DCSL-BETEXT-ITEM                     
198600       MOVE DCS-KDAKDISP-BULK     TO DCSL-BETEXT-OLDDATA                  
198700       MOVE MID-KDAKDISP-BULK-IN  TO DCS-KDAKDISP-BULK                    
198800                                     DCSL-BETEXT-NEWDATA                  
198900       ADD +1                     TO FIL-IDSEKVNR                         
199000       PERFORM IMS-ISRT-WDR601                                            
199100     END-IF                                                               
199200                                                                          
199300     IF MID-FLCLEAR-BULK-IN NOT = ALL '+'                                 
199400       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLCLEAR-BULK-IN-ATTR             
199500       MOVE 'FLCLEAR-BULK'        TO DCSL-BETEXT-ITEM                     
199600       MOVE DCS-FLCLEAR-BULK      TO DCSL-BETEXT-OLDDATA                  
199700       MOVE MID-FLCLEAR-BULK-IN   TO DCS-FLCLEAR-BULK                     
199800                                     DCSL-BETEXT-NEWDATA                  
199900       ADD +1                     TO FIL-IDSEKVNR                         
200000       PERFORM IMS-ISRT-WDR601                                            
200100     END-IF                                                               
200200                                                                          
200300     IF MID-KDSKRMET-IN NOT = ALL '+'                                     
200400       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDSKRMET-IN-ATTR                 
200500       MOVE 'KDSKRMET' TO DCSL-BETEXT-ITEM                                
200600       MOVE DCS-KDSKRMET         TO DCSL-BETEXT-OLDDATA                   
200700       MOVE MID-KDSKRMET-IN   TO DCS-KDSKRMET                             
200800                                 DCSL-BETEXT-NEWDATA                      
200900       ADD +1                 TO FIL-IDSEKVNR                             
201000       PERFORM IMS-ISRT-WDR601                                            
201100     END-IF                                                               
201200                                                                          
201300     IF MID-KVDAGAR-CROSS-IN NOT = ALL '+'                                
201400       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVDAGAR-CROSS-IN-ATTR            
201500       MOVE 'KVDAGAR-CROSS' TO DCSL-BETEXT-ITEM                           
201600       MOVE DCS-KVDAGAR-CROSS    TO DCSL-BETEXT-OLDDATA                   
201700       MOVE MID-KVDAGAR-CROSS-IN TO DCS-KVDAGAR-CROSS                     
201800                                 DCSL-BETEXT-NEWDATA                      
201900       ADD +1                 TO FIL-IDSEKVNR                             
202000       PERFORM IMS-ISRT-WDR601                                            
202100     END-IF                                                               
202200                                                                          
202300     IF MID-REQXBRYT-IN NOT = ALL '+'                                     
202400       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-REQXBRYT-IN-ATTR                 
202500       MOVE 'REQXBRYT' TO DCSL-BETEXT-ITEM                                
202600       MOVE DCS-REQXBRYT         TO DCSL-BETEXT-OLDDATA                   
202700       MOVE MID-REQXBRYT-IN   TO DCS-REQXBRYT                             
202800                                 DCSL-BETEXT-NEWDATA                      
202900       ADD +1                 TO FIL-IDSEKVNR                             
203000       PERFORM IMS-ISRT-WDR601                                            
203100     END-IF                                                               
203200                                                                          
203300     IF MID-REWILSON-IN NOT = ALL '+'                                     
203400       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-REWILSON-IN-ATTR                 
203500       MOVE 'REWILSON' TO DCSL-BETEXT-ITEM                                
203600       MOVE DCS-REWILSON      TO WS-REWILSON                              
203700       MOVE WS-REWILSON       TO DCSL-BETEXT-OLDDATA                      
203800       MOVE MID-REWILSON-IN   TO DCSL-BETEXT-NEWDATA                      
203900       MOVE WS-REWILSON-P     TO DCS-REWILSON                             
204000       ADD +1                 TO FIL-IDSEKVNR                             
204100       PERFORM IMS-ISRT-WDR601                                            
204200     END-IF                                                               
204300                                                                          
204400     IF MID-FLKNDVAL-IN NOT = ALL '+'                                     
204500       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLKNDVAL-IN-ATTR                 
204600       MOVE 'FLKNDVAL'         TO DCSL-BETEXT-ITEM                        
204700       MOVE DCS-FLKNDVAL       TO DCSL-BETEXT-OLDDATA                     
204800       IF MID-FLKNDVAL-IN = YES                                           
204900          MOVE JA              TO DCS-FLKNDVAL                            
205000       ELSE                                                               
205100          MOVE MID-FLKNDVAL-IN TO DCS-FLKNDVAL                            
205200       END-IF                                                             
205300       MOVE MID-FLKNDVAL-IN    TO DCSL-BETEXT-NEWDATA                     
205400       ADD +1                  TO FIL-IDSEKVNR                            
205500       PERFORM IMS-ISRT-WDR601                                            
205600     END-IF                                                               
205700                                                                          
205800     IF MID-IDDC-REF-IN NOT = ALL '+'                                     
205900       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDDC-REF-IN-ATTR                 
206000       MOVE 'IDDC-REF'        TO DCSL-BETEXT-ITEM                         
206100       MOVE DCS-IDDC-REF      TO DCSL-BETEXT-OLDDATA                      
206200                                 PARM-IDDC-REF-OLD                        
206300       MOVE MID-IDDC-REF-IN   TO DCS-IDDC-REF                             
206400                                 DCSL-BETEXT-NEWDATA                      
206500                                 PARM-IDDC-REF-NEW                        
206600       ADD +1                 TO FIL-IDSEKVNR                             
206700*      PERFORM IMS-ISRT-WDR601                                            
206800       MOVE W-IDDC            TO PARM-IDDC                                
206900*** ROUTINE W271B9 SHOULD NOT BE RUN IN PROD/JOHAN N                      
207000***    PERFORM S01-START-RTN-W271B9                                       
207100     END-IF                                                               
207200                                                                          
207300     IF MID-KDFAKTDC-IN NOT = ALL '+'                                     
207400       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDFAKTDC-IN-ATTR                 
207500       MOVE 'KDFAKTDC'        TO DCSL-BETEXT-ITEM                         
207600       MOVE DCS-KDFAKTDC      TO DCSL-BETEXT-OLDDATA                      
207700       MOVE MID-KDFAKTDC-IN   TO DCS-KDFAKTDC                             
207800                                 DCSL-BETEXT-NEWDATA                      
207900       ADD +1                 TO FIL-IDSEKVNR                             
208000       PERFORM IMS-ISRT-WDR601                                            
208100     END-IF                                                               
208200                                                                          
208300     IF MID-TID-RETOS-IN NOT = ALL '+'                                    
208400       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TID-RETOS-IN-ATTR                
208500       MOVE 'TID-RETOS'       TO DCSL-BETEXT-ITEM                         
208600       MOVE DCS-TID-RETOS     TO DCSL-BETEXT-OLDDATA                      
208700       EVALUATE MID-TID-RETOS-IN                                          
208800         WHEN 'N'                                                         
208900           MOVE ZERO            TO DCS-TID-RETOS                          
209000                                   DCSL-BETEXT-NEWDATA                    
209100         WHEN 'MO'                                                        
209200           MOVE +7              TO DCS-TID-RETOS                          
209300                                   DCSL-BETEXT-NEWDATA                    
209400         WHEN 'TU'                                                        
209500           MOVE +2              TO DCS-TID-RETOS                          
209600                                   DCSL-BETEXT-NEWDATA                    
209700         WHEN 'WE'                                                        
209800           MOVE +3              TO DCS-TID-RETOS                          
209900                                   DCSL-BETEXT-NEWDATA                    
210000         WHEN 'TH'                                                        
210100           MOVE +4              TO DCS-TID-RETOS                          
210200                                   DCSL-BETEXT-NEWDATA                    
210300         WHEN 'FR'                                                        
210400           MOVE +5              TO DCS-TID-RETOS                          
210500                                   DCSL-BETEXT-NEWDATA                    
210600       END-EVALUATE                                                       
210700       ADD +1                 TO FIL-IDSEKVNR                             
210800       PERFORM IMS-ISRT-WDR601                                            
210900     END-IF                                                               
211000                                                                          
211100     IF MID-TID-RET98-IN NOT = ALL '+'                                    
211200       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TID-RET98-IN-ATTR                
211300       MOVE 'TID-RET98'       TO DCSL-BETEXT-ITEM                         
211400       MOVE DCS-TID-RET98     TO DCSL-BETEXT-OLDDATA                      
211500       EVALUATE MID-TID-RET98-IN                                          
211600         WHEN 'N'                                                         
211700           MOVE ZERO            TO DCS-TID-RET98                          
211800                                   DCSL-BETEXT-NEWDATA                    
211900         WHEN 'MO'                                                        
212000           MOVE +7              TO DCS-TID-RET98                          
212100                                   DCSL-BETEXT-NEWDATA                    
212200         WHEN 'TU'                                                        
212300           MOVE +2              TO DCS-TID-RET98                          
212400                                   DCSL-BETEXT-NEWDATA                    
212500         WHEN 'WE'                                                        
212600           MOVE +3              TO DCS-TID-RET98                          
212700                                   DCSL-BETEXT-NEWDATA                    
212800         WHEN 'TH'                                                        
212900           MOVE +4              TO DCS-TID-RET98                          
213000                                   DCSL-BETEXT-NEWDATA                    
213100         WHEN 'FR'                                                        
213200           MOVE +5              TO DCS-TID-RET98                          
213300                                   DCSL-BETEXT-NEWDATA                    
213400       END-EVALUATE                                                       
213500       ADD +1                 TO FIL-IDSEKVNR                             
213600       PERFORM IMS-ISRT-WDR601                                            
213700     END-IF                                                               
213800                                                                          
213900     IF MID-PRARTSTD-SKRLO98-IN NOT = ALL '+'                             
214000       MOVE MFS-ADD-LYS-UPP-FAELT  TO MOD-PRARTSTD-SKRLO98-IN-ATTR        
214100       MOVE 'PRARTSTD-SKRLO98'      TO DCSL-BETEXT-ITEM                   
214200       MOVE DCS-PRARTSTD-SKRLO98    TO WS-PRARTSTD                        
214300       MOVE WS-PRARTSTD             TO DCSL-BETEXT-OLDDATA                
214400       MOVE MID-PRARTSTD-SKRLO98-IN TO DCSL-BETEXT-NEWDATA                
214500       MOVE MID-PRARTSTD-SKRLO98-IN TO DCS-PRARTSTD-SKRLO98               
214600       ADD +1                       TO FIL-IDSEKVNR                       
214700       PERFORM IMS-ISRT-WDR601                                            
214800     END-IF                                                               
214900                                                                          
215000     IF MID-FLTRLO88-MAN-IN NOT   = ALL '+'                               
215100       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLTRLO88-MAN-IN-ATTR             
215200       MOVE 'FLTRLO88-MAN'        TO DCSL-BETEXT-ITEM                     
215300       MOVE DCS-FLTRLO88-MAN      TO DCSL-BETEXT-OLDDATA                  
215400       EVALUATE MID-FLTRLO88-MAN-IN                                       
215500         WHEN   'N'                                                       
215600           MOVE 'N'               TO DCS-FLTRLO88-MAN                     
215700                                     DCSL-BETEXT-NEWDATA                  
215800         WHEN   'Y'                                                       
215900           MOVE 'Y'               TO DCS-FLTRLO88-MAN                     
216000                                     DCSL-BETEXT-NEWDATA                  
216100       END-EVALUATE                                                       
216200       ADD +1                     TO FIL-IDSEKVNR                         
216300       PERFORM IMS-ISRT-WDR601                                            
216400     END-IF                                                               
216500                                                                          
216600     IF MID-FLTRLO88-TIS-IN NOT   = ALL '+'                               
216700       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLTRLO88-TIS-IN-ATTR             
216800       MOVE 'FLTRLO88-TIS'        TO DCSL-BETEXT-ITEM                     
216900       MOVE DCS-FLTRLO88-TIS      TO DCSL-BETEXT-OLDDATA                  
217000       EVALUATE MID-FLTRLO88-TIS-IN                                       
217100         WHEN   'N'                                                       
217200           MOVE 'N'               TO DCS-FLTRLO88-TIS                     
217300                                     DCSL-BETEXT-NEWDATA                  
217400         WHEN   'Y'                                                       
217500           MOVE 'Y'               TO DCS-FLTRLO88-TIS                     
217600                                   DCSL-BETEXT-NEWDATA                    
217700       END-EVALUATE                                                       
217800       ADD +1                     TO FIL-IDSEKVNR                         
217900       PERFORM IMS-ISRT-WDR601                                            
218000     END-IF                                                               
218100                                                                          
218200     IF MID-FLTRLO88-ONS-IN NOT   = ALL '+'                               
218300       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLTRLO88-ONS-IN-ATTR             
218400       MOVE 'FLTRLO88-ONS'        TO DCSL-BETEXT-ITEM                     
218500       MOVE DCS-FLTRLO88-ONS      TO DCSL-BETEXT-OLDDATA                  
218600       EVALUATE MID-FLTRLO88-ONS-IN                                       
218700         WHEN   'N'                                                       
218800           MOVE 'N'               TO DCS-FLTRLO88-ONS                     
218900                                     DCSL-BETEXT-NEWDATA                  
219000         WHEN   'Y'                                                       
219100           MOVE 'Y'               TO DCS-FLTRLO88-ONS                     
219200                                     DCSL-BETEXT-NEWDATA                  
219300       END-EVALUATE                                                       
219400       ADD +1                     TO FIL-IDSEKVNR                         
219500       PERFORM IMS-ISRT-WDR601                                            
219600     END-IF                                                               
219700                                                                          
219800     IF MID-FLTRLO88-TOR-IN NOT   = ALL '+'                               
219900       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLTRLO88-TOR-IN-ATTR             
220000       MOVE 'FLTRLO88-TOR'        TO DCSL-BETEXT-ITEM                     
220100       MOVE DCS-FLTRLO88-TOR      TO DCSL-BETEXT-OLDDATA                  
220200       EVALUATE MID-FLTRLO88-TOR-IN                                       
220300         WHEN   'N'                                                       
220400           MOVE 'N'               TO DCS-FLTRLO88-TOR                     
220500                                     DCSL-BETEXT-NEWDATA                  
220600         WHEN   'Y'                                                       
220700           MOVE 'Y'               TO DCS-FLTRLO88-TOR                     
220800                                     DCSL-BETEXT-NEWDATA                  
220900       END-EVALUATE                                                       
221000       ADD +1                     TO FIL-IDSEKVNR                         
221100       PERFORM IMS-ISRT-WDR601                                            
221200     END-IF                                                               
221300                                                                          
221400     IF MID-FLTRLO88-FRE-IN NOT   = ALL '+'                               
221500       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLTRLO88-FRE-IN-ATTR             
221600       MOVE 'FLTRLO88-FRE'        TO DCSL-BETEXT-ITEM                     
221700       MOVE DCS-FLTRLO88-FRE      TO DCSL-BETEXT-OLDDATA                  
221800       EVALUATE MID-FLTRLO88-FRE-IN                                       
221900         WHEN   'N'                                                       
222000           MOVE 'N'               TO DCS-FLTRLO88-FRE                     
222100                                     DCSL-BETEXT-NEWDATA                  
222200         WHEN   'Y'                                                       
222300           MOVE 'Y'               TO DCS-FLTRLO88-FRE                     
222400                                     DCSL-BETEXT-NEWDATA                  
222500       END-EVALUATE                                                       
222600       ADD +1                     TO FIL-IDSEKVNR                         
222700       PERFORM IMS-ISRT-WDR601                                            
222800     END-IF                                                               
222900                                                                          
223000     PERFORM IMS-REPL-WDB601                                              
223100                                                                          
223200     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
223300     CALL WMEDKONV USING MED-WMEDAREA                                     
223400     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
223500     PERFORM MFS-FORM-ATTR                                                
223600     PERFORM MFS-RENSA-FAELT-IN                                           
223700     .                                                                    
223800     EJECT                                                                
223900                                                                          
224000 S01-START-RTN-W271B9 SECTION.                                            
224100                                                                          
224200     MOVE '4403'          TO MSGSOP-IDTRANS                               
224300     MOVE MFS-KDMFSFOR    TO MSGSOP-KDMFSFOR                              
224400     MOVE 'W271B9    '    TO MSGSOP-IDPROCESS                             
224500     MOVE 'O'             TO MSGSOP-KDSOPFUNK                             
224600     MOVE PARM-TESYMBV    TO MSGSOP-TESYMBV                               
224700     PERFORM IMS-INSERT-ALT-MSG                                           
224800                                                                          
224900     SKIP3                                                                
225000     .                                                                    
225100 MFS-RENSA-FAELT-UT SECTION.                                              
225200                                                                          
225300*    --- ALLA UTDATA-FÄLT                                                 
225400     MOVE MFS-RENSA-FAELT TO MOD-FLDCRET                                  
225500                             MOD-FLINLHIST                                
225600                             MOD-FLFSEDEL                                 
225700                             MOD-FLINLREP                                 
225800                             MOD-FLPRISSPR                                
225900                             MOD-FLBINNUT                                 
226000                             MOD-FLSAMPAK                                 
226100                             MOD-FLSEASBER                                
226200                             MOD-FLTYP6JU                                 
226300                             MOD-FLRSI                                    
226400                             MOD-KDPORDL                                  
226500                             MOD-IDLISTNR                                 
226600                             MOD-FLEXCP1-PRIO                             
226700                             MOD-FLEXCP1-REFBEO                           
226800                             MOD-FLEXCP2-REFBEO                           
226900                             MOD-FLEXCP3-REFBEO                           
227000                             MOD-FLEXCP4-REFBEO                           
227100                             MOD-FLEXCP1-REFBER                           
227200                             MOD-FLEXCP2-REFBER                           
227300                             MOD-FLOVRLAGBER                              
                                   MOD-FLARTADD                                 
227400                             MOD-KVINVAUT                                 
227500                             MOD-SUINVGRANS                               
227600                             MOD-IDPRTLST-INVA                            
227700                             MOD-IDPRTLST-INVAB                           
227800                             MOD-IDPRTLST-INL                             
227900                             MOD-IDPRTLST-INLA                            
228000                             MOD-KDDCSTYR-BUY                             
228100                             MOD-KDDCSTYR-KUND                            
228200                             MOD-KDDCSTYR-REFTAB                          
228300                             MOD-KVDAGAR-POKS                             
228400                             MOD-KVDAGAR-PP                               
228500                             MOD-KDAKDISP-DAG                             
228600                             MOD-KDAKDISP-BULK                            
228700                             MOD-FLCLEAR-BULK                             
228800                             MOD-KDSKRMET                                 
228900                             MOD-KVDAGAR-CROSS                            
229000                             MOD-REQXBRYT                                 
229100                             MOD-REWILSON                                 
229200                             MOD-FLKNDVAL                                 
229300                             MOD-KDFAKTDC                                 
229400                             MOD-IDDC-REF                                 
229500                             MOD-TID-RETOS                                
229600                             MOD-TID-RET98                                
229700                             MOD-PRARTSTD-SKRLO98                         
229800                             MOD-FLTRLO88-MAN                             
229900                             MOD-FLTRLO88-TIS                             
230000                             MOD-FLTRLO88-ONS                             
230100                             MOD-FLTRLO88-TOR                             
230200                             MOD-FLTRLO88-FRE                             
230300     .                                                                    
230400     EJECT                                                                
230500 MFS-RENSA-FAELT-IN SECTION.                                              
230600                                                                          
230700*    --- ALLA INDATA-FÄLT                                                 
230800     MOVE MFS-RENSA-FAELT TO MOD-FLDCRET-IN                               
230900                             MOD-FLINLHIST-IN                             
231000                             MOD-FLFSEDEL-IN                              
231100                             MOD-FLINLREP-IN                              
231200                             MOD-FLPRISSPR-IN                             
231300                             MOD-FLBINNUT-IN                              
231400                             MOD-FLSAMPAK-IN                              
231500                             MOD-FLSEASBER-IN                             
231600                             MOD-FLTYP6JU-IN                              
231700                             MOD-FLRSI-IN                                 
231800                             MOD-KDPORDL-IN                               
231900                             MOD-IDLISTNR-IN                              
232000                             MOD-FLEXCP1-PRIO-IN                          
232100                             MOD-FLEXCP1-REFBEO-IN                        
232200                             MOD-FLEXCP2-REFBEO-IN                        
232300                             MOD-FLEXCP3-REFBEO-IN                        
232400                             MOD-FLEXCP4-REFBEO-IN                        
232500                             MOD-FLEXCP1-REFBER-IN                        
232600                             MOD-FLEXCP2-REFBER-IN                        
232700                             MOD-FLOVRLAGBER-IN                           
                                   MOD-FLARTADD-IN                              
232800                             MOD-KVINVAUT-IN                              
232900                             MOD-SUINVGRANS-IN                            
233000                             MOD-IDPRTLST-INVA-IN                         
233100                             MOD-IDPRTLST-INVAB-IN                        
233200                             MOD-IDPRTLST-INL-IN                          
233300                             MOD-IDPRTLST-INLA-IN                         
233400                             MOD-KDDCSTYR-BUY-IN                          
233500                             MOD-KDDCSTYR-KUND-IN                         
233600                             MOD-KDDCSTYR-REFTAB-IN                       
233700                             MOD-KVDAGAR-POKS-IN                          
233800                             MOD-KVDAGAR-PP-IN                            
233900                             MOD-KDAKDISP-DAG-IN                          
234000                             MOD-KDAKDISP-BULK-IN                         
234100                             MOD-FLCLEAR-BULK-IN                          
234200                             MOD-KDSKRMET-IN                              
234300                             MOD-KVDAGAR-CROSS-IN                         
234400                             MOD-REQXBRYT-IN                              
234500                             MOD-REWILSON-IN                              
234600                             MOD-FLKNDVAL-IN                              
234700                             MOD-KDFAKTDC-IN                              
234800                             MOD-IDDC-REF-IN                              
234900                             MOD-TID-RETOS-IN                             
235000                             MOD-TID-RET98-IN                             
235100                             MOD-PRARTSTD-SKRLO98-IN                      
235200                             MOD-FLTRLO88-MAN-IN                          
235300                             MOD-FLTRLO88-TIS-IN                          
235400                             MOD-FLTRLO88-ONS-IN                          
235500                             MOD-FLTRLO88-TOR-IN                          
235600                             MOD-FLTRLO88-FRE-IN                          
235700     .                                                                    
235800     EJECT                                                                
235900 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
236000                                                                          
236100*    --- ALLA UTDATA-FÄLT                                                 
236200     MOVE MFS-ROER-EJ-FAELT TO MOD-FLDCRET                                
236300                               MOD-FLINLHIST                              
236400                               MOD-FLFSEDEL                               
236500                               MOD-FLINLREP                               
236600                               MOD-FLPRISSPR                              
236700                               MOD-FLBINNUT                               
236800                               MOD-FLSAMPAK                               
236900                               MOD-FLSEASBER                              
237000                               MOD-FLTYP6JU                               
237100                               MOD-FLRSI                                  
237200                               MOD-KDPORDL                                
237300                               MOD-IDLISTNR                               
237400                               MOD-FLEXCP1-PRIO                           
237500                               MOD-FLEXCP1-REFBEO                         
237600                               MOD-FLEXCP2-REFBEO                         
237700                               MOD-FLEXCP3-REFBEO                         
237800                               MOD-FLEXCP4-REFBEO                         
237900                               MOD-FLEXCP1-REFBER                         
238000                               MOD-FLEXCP2-REFBER                         
238100                               MOD-FLOVRLAGBER                            
                                     MOD-FLARTADD                               
238200                               MOD-KVINVAUT                               
238300                               MOD-SUINVGRANS                             
238400                               MOD-IDPRTLST-INVA                          
238500                               MOD-IDPRTLST-INVAB                         
238600                               MOD-IDPRTLST-INL                           
238700                               MOD-IDPRTLST-INLA                          
238800                               MOD-KDDCSTYR-BUY                           
238900                               MOD-KDDCSTYR-KUND                          
239000                               MOD-KDDCSTYR-REFTAB                        
239100                               MOD-KVDAGAR-POKS                           
239200                               MOD-KVDAGAR-PP                             
239300                               MOD-KDAKDISP-DAG                           
239400                               MOD-KDAKDISP-BULK                          
239500                               MOD-FLCLEAR-BULK                           
239600                               MOD-KDSKRMET                               
239700                               MOD-KVDAGAR-CROSS                          
239800                               MOD-REQXBRYT                               
239900                               MOD-REWILSON                               
240000                               MOD-FLKNDVAL                               
240100                               MOD-KDFAKTDC                               
240200                               MOD-IDDC-REF                               
240300                               MOD-TID-RETOS                              
240400                               MOD-TID-RET98                              
240500                               MOD-PRARTSTD-SKRLO98                       
240600                               MOD-FLTRLO88-MAN                           
240700                               MOD-FLTRLO88-TIS                           
240800                               MOD-FLTRLO88-ONS                           
240900                               MOD-FLTRLO88-TOR                           
241000                               MOD-FLTRLO88-FRE                           
241100     .                                                                    
241200     EJECT                                                                
241300 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
241400                                                                          
241500*    --- ALLA INDATA-FÄLT                                                 
241600     MOVE MFS-ROER-EJ-FAELT TO MOD-FLDCRET-IN                             
241700                               MOD-FLINLHIST-IN                           
241800                               MOD-FLFSEDEL-IN                            
241900                               MOD-FLINLREP-IN                            
242000                               MOD-FLPRISSPR-IN                           
242100                               MOD-FLBINNUT-IN                            
242200                               MOD-FLSAMPAK-IN                            
242300                               MOD-FLSEASBER-IN                           
242400                               MOD-FLTYP6JU-IN                            
242500                               MOD-FLRSI-IN                               
242600                               MOD-KDPORDL-IN                             
242700                               MOD-IDLISTNR-IN                            
242800                               MOD-FLEXCP1-PRIO-IN                        
242900                               MOD-FLEXCP1-REFBEO-IN                      
243000                               MOD-FLEXCP2-REFBEO-IN                      
243100                               MOD-FLEXCP3-REFBEO-IN                      
243200                               MOD-FLEXCP4-REFBEO-IN                      
243300                               MOD-FLEXCP1-REFBER-IN                      
243400                               MOD-FLEXCP2-REFBER-IN                      
243500                               MOD-FLOVRLAGBER-IN                         
                                     MOD-FLARTADD-IN                            
243600                               MOD-KVINVAUT-IN                            
243700                               MOD-SUINVGRANS-IN                          
243800                               MOD-IDPRTLST-INVA-IN                       
243900                               MOD-IDPRTLST-INVAB-IN                      
244000                               MOD-IDPRTLST-INL-IN                        
244100                               MOD-IDPRTLST-INLA-IN                       
244200                               MOD-KDDCSTYR-BUY-IN                        
244300                               MOD-KDDCSTYR-KUND-IN                       
244400                               MOD-KDDCSTYR-REFTAB-IN                     
244500                               MOD-KVDAGAR-POKS-IN                        
244600                               MOD-KVDAGAR-PP-IN                          
244700                               MOD-KDAKDISP-DAG-IN                        
244800                               MOD-KDAKDISP-BULK-IN                       
244900                               MOD-FLCLEAR-BULK-IN                        
245000                               MOD-KDSKRMET-IN                            
245100                               MOD-KVDAGAR-CROSS-IN                       
245200                               MOD-REQXBRYT-IN                            
245300                               MOD-REWILSON-IN                            
245400                               MOD-FLKNDVAL-IN                            
245500                               MOD-KDFAKTDC-IN                            
245600                               MOD-IDDC-REF-IN                            
245700                               MOD-TID-RETOS-IN                           
245800                               MOD-TID-RET98-IN                           
245900                               MOD-PRARTSTD-SKRLO98-IN                    
246000                               MOD-FLTRLO88-MAN-IN                        
246100                               MOD-FLTRLO88-TIS-IN                        
246200                               MOD-FLTRLO88-ONS-IN                        
246300                               MOD-FLTRLO88-TOR-IN                        
246400                               MOD-FLTRLO88-FRE-IN                        
246500     .                                                                    
246600     EJECT                                                                
246700 MFS-FORM-ATTR SECTION.                                                   
246800                                                                          
246900*    --- ALLA INDATA-FÄLT                                                 
247000     MOVE MFS-FORMATETS-ATTR TO MOD-FLDCRET-IN-ATTR                       
247100                                MOD-FLINLHIST-IN-ATTR                     
247200                                MOD-FLFSEDEL-IN-ATTR                      
247300                                MOD-FLINLREP-IN-ATTR                      
247400                                MOD-FLPRISSPR-IN-ATTR                     
247500                                MOD-FLBINNUT-IN-ATTR                      
247600                                MOD-FLSAMPAK-IN-ATTR                      
247700                                MOD-FLSEASBER-IN-ATTR                     
247800                                MOD-FLTYP6JU-IN-ATTR                      
247900                                MOD-FLRSI-IN-ATTR                         
248000                                MOD-KDPORDL-IN-ATTR                       
248100                                MOD-IDLISTNR-IN-ATTR                      
248200                                MOD-FLEXCP1-PRIO-IN-ATTR                  
248300                                MOD-FLEXCP1-REFBEO-IN-ATTR                
248400                                MOD-FLEXCP2-REFBEO-IN-ATTR                
248500                                MOD-FLEXCP3-REFBEO-IN-ATTR                
248600                                MOD-FLEXCP4-REFBEO-IN-ATTR                
248700                                MOD-FLEXCP1-REFBER-IN-ATTR                
248800                                MOD-FLEXCP2-REFBER-IN-ATTR                
248900                                MOD-FLOVRLAGBER-IN-ATTR                   
                                      MOD-FLARTADD-IN-ATTR                      
249000                                MOD-KVINVAUT-IN-ATTR                      
249100                                MOD-SUINVGRANS-IN-ATTR                    
249200                                MOD-IDPRTLST-INVA-IN-ATTR                 
249300                                MOD-IDPRTLST-INVAB-IN-ATTR                
249400                                MOD-IDPRTLST-INL-IN-ATTR                  
249500                                MOD-IDPRTLST-INLA-IN-ATTR                 
249600                                MOD-KDDCSTYR-BUY-IN-ATTR                  
249700                                MOD-KDDCSTYR-KUND-IN-ATTR                 
249800                                MOD-KDDCSTYR-REFTAB-IN-ATTR               
249900                                MOD-KVDAGAR-POKS-IN-ATTR                  
250000                                MOD-KVDAGAR-PP-IN-ATTR                    
250100                                MOD-KDAKDISP-DAG-IN-ATTR                  
250200                                MOD-KDAKDISP-BULK-IN-ATTR                 
250300                                MOD-FLCLEAR-BULK-IN-ATTR                  
250400                                MOD-KDSKRMET-IN-ATTR                      
250500                                MOD-KVDAGAR-CROSS-IN-ATTR                 
250600                                MOD-REQXBRYT-IN-ATTR                      
250700                                MOD-REWILSON-IN-ATTR                      
250800                                MOD-FLKNDVAL-IN-ATTR                      
250900                                MOD-KDFAKTDC-IN-ATTR                      
251000                                MOD-IDDC-REF-IN-ATTR                      
251100                                MOD-TID-RETOS-IN-ATTR                     
251200                                MOD-TID-RET98-IN-ATTR                     
251300                                MOD-PRARTSTD-SKRLO98-IN-ATTR              
251400                                MOD-FLTRLO88-MAN-IN-ATTR                  
251500                                MOD-FLTRLO88-TIS-IN-ATTR                  
251600                                MOD-FLTRLO88-ONS-IN-ATTR                  
251700                                MOD-FLTRLO88-TOR-IN-ATTR                  
251800                                MOD-FLTRLO88-FRE-IN-ATTR                  
251900     .                                                                    
252000     EJECT                                                                
252100* --- IMS SEKTIONER ---                                                   
252200     SKIP3                                                                
252300 IMS-GET-MSG SECTION.                                                     
252400                                                                          
252500     MOVE '  QC' TO GODK-STATUSKODER                                      
252600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
252700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
252800     PERFORM IMS-STATUSKONTROLL                                           
252900     .                                                                    
253000     SKIP3                                                                
253100 IMS-INSERT-MSG SECTION.                                                  
253200                                                                          
253300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
253400     MOVE SPACE TO GODK-STATUSKODER                                       
253500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
253600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
253700     PERFORM IMS-STATUSKONTROLL                                           
253800     .                                                                    
253900     EJECT                                                                
254000 IMS-INSERT-ALT-MSG SECTION.                                              
254100                                                                          
254200     MOVE SPACE TO GODK-STATUSKODER                                       
254300     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW                    
254400     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
254500     PERFORM IMS-STATUSKONTROLL                                           
254600     .                                                                    
254700     EJECT                                                                
254800 IMS-GU-WDB601 SECTION.                                                   
254900                                                                          
255000     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
255100          DELIMITED BY SIZE INTO SSA1                                     
255200     MOVE '  GE' TO GODK-STATUSKODER                                      
255300     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
255400     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
255500     PERFORM IMS-STATUSKONTROLL                                           
255600     .                                                                    
255700     SKIP3                                                                
255800 IMS-GHU-WDB601 SECTION.                                                  
255900                                                                          
256000     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
256100          DELIMITED BY SIZE INTO SSA1                                     
256200     MOVE '  ' TO GODK-STATUSKODER                                        
256300     CALL CBLTDLI USING GHU WDB6-PCB DLI-IO-WDB601 SSA1                   
256400     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
256500     PERFORM IMS-STATUSKONTROLL                                           
256600     .                                                                    
256700     SKIP3                                                                
256810 IMS-REPL-WDB601 SECTION.                                                 
256900                                                                          
257000     MOVE '  ' TO GODK-STATUSKODER                                        
257100     CALL CBLTDLI USING REPL WDB6-PCB DLI-IO-WDB601                       
257200     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
257300     PERFORM IMS-STATUSKONTROLL                                           
257400     .                                                                    
257500     EJECT                                                                
257510 IMS-GU-WDB615 SECTION.                                                   
257520                                                                          
257530     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
257540          DELIMITED BY SIZE INTO SSA1                                     
257550     STRING 'WDB615  (WDB615KY =' W-WDB615KY-X ')'                        
257560          DELIMITED BY SIZE INTO SSA2                                     
257570     MOVE '  GE' TO GODK-STATUSKODER                                      
257580     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB615 SSA1 SSA2               
257590     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
257591     PERFORM IMS-STATUSKONTROLL                                           
257592     .                                                                    
257593     SKIP3                                                                
257594 IMS-GU-WDB616 SECTION.                                                   
257595                                                                          
257596     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
257597          DELIMITED BY SIZE INTO SSA1                                     
257598     STRING 'WDB616  (IDDCREF  =' W-IDDC-REF-X ')'                        
257599          DELIMITED BY SIZE INTO SSA2                                     
257600     MOVE '  GE' TO GODK-STATUSKODER                                      
257601     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB616 SSA1 SSA2               
257602     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
257603     PERFORM IMS-STATUSKONTROLL                                           
257604     .                                                                    
257605     SKIP3                                                                
257610 IMS-ISRT-WDR601 SECTION.                                                 
257700                                                                          
257800     MOVE 'WDR601 ' TO SSA1                                               
257900     MOVE '    ' TO GODK-STATUSKODER                                      
258000     CALL CBLTDLI USING ISRT WDR6-PCB DLI-IO-WDR601 SSA1                  
258100     MOVE WDR6-STATUS-CODE TO STATUS-WS                                   
258200     PERFORM IMS-STATUSKONTROLL                                           
258300     .                                                                    
258400     SKIP3                                                                
258500 IMS-STATUSKONTROLL SECTION.                                              
258600                                                                          
258700     SET STATUS-IX TO 1                                                   
258800     SEARCH GODK-STATUS                                                   
258900       AT END                                                             
259000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
259100         DELIMITED BY SIZE INTO FELTEXT                                   
259200         CALL FELLOG                                                      
259300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
259400         CONTINUE                                                         
259500     END-SEARCH                                                           
259600     .                                                                    
