000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2040200.                                                
000300 AUTHOR.         KJELLSON GÖRAN.                                          
000400 DATE-WRITTEN.   12/07/12.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        LAGERINFO - ANSKAFFNING -KINA                                    
000900*                                                                         
001000*        PROGRAMMET LÄSER      WDK6                                       
001100*        PROGRAMMET LÄSER      WDK7                                       
001200*        PROGRAMMET LÄSER      WDL7                                       
001300*        PROGRAMMET LÄSER      WDD3                                       
001400*        PROGRAMMET LÄSER      WDD9                                       
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSAKTION: W2T402                                              
001800*        MID:         W2I40201                                            
001900*                                                                         
002000*    UTDATA.                                                              
002100*        MOD:         W2O40201                                            
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500                                                                          
002600 DATA DIVISION.                                                           
002700     EJECT                                                                
002800 WORKING-STORAGE SECTION.                                                 
002900 77  IDPGM                       PIC X(08)   VALUE 'W2040200'.            
003000                                                                          
003100*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003200 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003300                                                                          
003400 77  JA                          PIC X       VALUE 'J'.                   
003500 77  NEJ                         PIC X       VALUE 'N'.                   
003600 77  YES                         PIC X       VALUE 'Y'.                   
003700 77  NOO                         PIC X       VALUE 'N'.                   
003800                                                                          
003900 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
004000 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
004100                                                                          
004200*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004300                                                                          
004400 01  WS-TODAYS-AAAAMMDD          PIC 9(8).                                
004500 01  FILLER REDEFINES WS-TODAYS-AAAAMMDD.                                 
004600     03  WS-TODAYS-SS            PIC 9(2).                                
004700     03  WS-TODAYS-AAMMDD        PIC 9(6).                                
004800                                                                          
004900 01  WS-KDERS                    PIC  9(2)   VALUE ZERO.                  
005000 01  WS-KVREFOVL-CURRENT         PIC S9(7)   VALUE ZERO COMP-3.           
005100 01  WS-KVRESS-SUM               PIC S9(7)   VALUE ZERO COMP-3.           
005200 01  WS-STONHND-SUM              PIC S9(7)   VALUE ZERO COMP-3.           
005300 01  WS-KVPB-SUM                 PIC S9(7)V9 VALUE ZERO COMP-3.           
005400 01  WS-KVLS-SUM                 PIC S9(7)   VALUE ZERO COMP-3.           
005500 01  WS-KVOKS-SUM                PIC S9(7)   VALUE ZERO COMP-3.           
005600 01  WS-KVAKS-SDC-SUM            PIC S9(7)   VALUE ZERO COMP-3.           
005700 01  WS-KVAKS-PAV-SUM            PIC S9(7)   VALUE ZERO COMP-3.           
005800 01  WS-KVAVIS                   PIC S9(7)   VALUE ZERO COMP-3.           
005900 01  WS-KVROS-SUM                PIC S9(7)   VALUE ZERO COMP-3.           
006000 01  WS-KVUTRS-SUM               PIC S9(7)   VALUE ZERO COMP-3.           
006100 01  WS-KVEFRS-SUM               PIC S9(7)   VALUE ZERO COMP-3.           
006200 01  WS-KVSPANT-SUM              PIC S9(7)   VALUE ZERO COMP-3.           
006300 01  WS-KVREFOVL-SUM             PIC S9(7)   VALUE ZERO COMP-3.           
006400 01  W-SUM-KVBEART-Q             PIC S9(7)   VALUE ZERO COMP-3.           
006500 01  WS-WDK711-EXIST             PIC X(01)   VALUE SPACE.                 
006600 01  IDLEVNR-WS                  PIC X(05)   VALUE SPACE.                 
006700 01  WS-IDLANDX2                 PIC X(02)   VALUE SPACE.                 
006800                                                                          
006900 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007000     88  NYCKLAR-OK                          VALUE 'J'.                   
007100     88  NYCKLAR-FEL                         VALUE 'N'.                   
007200                                                                          
007300 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007400     88  EGEN-MID                            VALUE '2402'.                
007500     88  2471-MID                            VALUE '2471'.                
007600     88  GODK-MID                            VALUE '2401' '2402'          
007700                                                   '2403' '2404'          
007800                                                   '2405' '2406'          
007900                                                   '2407' '2408'          
008000                                                   '2409'.                
008100     88  HELP-MID                            VALUE '0551'.                
008200                                                                          
008300 01  WS-IDDC-REF-IX              PIC 9(2)    VALUE ZERO.                  
008400 01  WS-IDDC-REF-IX-MAX          PIC 9(2)    VALUE 50.                    
008500 01  WS-IDDC-REF-TAB.                                                     
008600     03  WS-IDDC-REF OCCURS 50   PIC X(2).                                
008700                                                                          
008800     EJECT                                                                
008900*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
009000 01  GENERELLA-SUBPROGRAM.                                                
009100     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
009200     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
009300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009500     03  W271REFL                PIC X(8)    VALUE 'W271REFL'.            
009600     03  W271UTUP                PIC X(8)    VALUE 'W271UTUP'.            
009700     EJECT                                                                
009800*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
009900*01 -COPY WMEDAREA                                                        
010000     EJECT                                                                
010100*    --- PARAMETRAR TILL W271REFL                                         
010200*01 -COPY W271REFL                                                        
010300     EJECT                                                                
010400*    --- PARAMETRAR TILL W271UTUP                                         
010500*01 -COPY W271UTUP                                                        
010600     EJECT                                                                
010700 01  MESSAGE-CODES.                                                       
010800     03  ERR-ARTIKEL-SAKNAS      PIC X(3)    VALUE '017'.                 
010900     03  ERR-DC-SAKNAS           PIC X(3)    VALUE '026'.                 
011000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
011100     EJECT                                                                
011200                                                                          
011300*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
011400*                                                                         
011500 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
011600     SKIP3                                                                
011700*01 -COPY WMSGINIT                                                        
011800     EJECT                                                                
011900*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
012000*                                                                         
012100 01  SPAR-AREA.                                                           
012200     03  SPAR-IDTRANS           PIC X(4)    VALUE '2402'.                 
012300     EJECT                                                                
012400*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
012500*                                                                         
012600 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
012700     SKIP3                                                                
012800*01  MID -COPY W2I40201                                                   
012900     EJECT                                                                
013000 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
013100     SKIP3                                                                
013200*01  -COPY WMSGAREA                                                       
013300     EJECT                                                                
013400     03  MOD REDEFINES MSG-AREA.                                          
013500*      05  -COPY W2O40201  -PRE MOD-                                      
013600     EJECT                                                                
013700 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
013800     SKIP3                                                                
013900*01  -COPY WMFSAREA                                                       
014000     EJECT                                                                
014100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
014200*                                                                         
014300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014400     SKIP3                                                                
014500 01  NYCKLAR-TILL-DLI.                                                    
014600     03  W-IDARTNR-X.                                                     
014700         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
014800     03  W-KDSEGKEY-X.                                                    
014900         05  W-KDSEGKEY          PIC X(1)    VALUE SPACE.                 
015000     03  W-IDHTYP-X.                                                      
015100         05  W-IDHTYP            PIC X(4)     VALUE '4541'.               
015200         05  FILLER              PIC X(26)    VALUE LOW-VALUE.            
015300     03  W-IDDC-X.                                                        
015400         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
015500     03  W-IDDC-B6-X.                                                     
015600         05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
015700     03  W-IDDC-K7-X.                                                     
015800         05  W-IDDC-K7           PIC X(2)    VALUE SPACE.                 
015900     03  W-IDLAND-X.                                                      
016000         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
016100     03  W-IDAVTAL-X.                                                     
016200         05  W-IDAVTAL           PIC S9(13)  VALUE ZERO COMP-3.           
016300     03  W-IDBENNR-X.                                                     
016400         05  W-IDBENNR           PIC S9(7)   VALUE ZERO COMP-3.           
016500     03  W-IDSKYLT-X.                                                     
016600         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
016700     03  W-WDD901KY-X.                                                    
016800         05  W-WDD901KY          PIC X(7)    VALUE SPACE.                 
016900     03  W-IDLEVNR-X.                                                     
017000         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
017100     03  W-W6D1HSEQ-X.                                                    
017200         05  W-IDARTNR-HSEQ      PIC S9(9)   VALUE ZERO COMP-3.           
017300     03  W-KDVORATG-X.                                                    
017400         05  W-KDVORATG          PIC X       VALUE '2'.                   
017500                                                                          
017600     SKIP2                                                                
017700*    --- STATUS KODER FRÅN IMS                                            
017800 01  STATUS-WS                   PIC XX.                                  
017900     88  SEGMENT-FINNS                       VALUE '  '.                  
018000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
018100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
018200     88  SEGMENT-SLUT                        VALUE 'GB'.                  
018300     SKIP2                                                                
018400 01  GODK-STATUSKODER.                                                    
018500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
018600     SKIP3                                                                
018700 01  ALL-SSA.                                                             
018800     03 SSA1                     PIC X(64).                               
018900     03 SSA2                     PIC X(64).                               
019000     03 SSA3                     PIC X(64).                               
019100     EJECT                                                                
019200*    --- IMS FUNKTIONSKODER                                               
019300*01  -COPY W0003                                                          
019400     EJECT                                                                
019500*    ---  DLI INPUT-OUTPUT AREA                                           
019600                                                                          
019700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
019800 01  DLI-IO-WDK601.                                                       
019900*    03  -COPY WDK601                                                     
020000     EJECT                                                                
020100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
020200 01  DLI-IO-WDK611.                                                       
020300*    03  -COPY WDK611                                                     
020400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK629'.                      
020500 01  DLI-IO-WDK629.                                                       
020600*    03  -COPY WDK629                                                     
020700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
020800 01  DLI-IO-WDK701.                                                       
020900*    03  -COPY WDK701                                                     
021000     EJECT                                                                
021100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
021200 01  DLI-IO-WDK711.                                                       
021300*    03  -COPY WDK711                                                     
021400     EJECT                                                                
021500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK712'.                      
021600 01  DLI-IO-WDK712.                                                       
021700*    03  -COPY WDK712                                                     
021800     EJECT                                                                
021900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK722'.                      
022000 01  DLI-IO-WDK722.                                                       
022100*    03  -COPY WDK722                                                     
022200     EJECT                                                                
022300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL711'.                      
022400 01  DLI-IO-WDL711.                                                       
022500*    03  -COPY WDL711                                                     
022600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD311'.                      
022700 01  DLI-IO-WDD311.                                                       
022800*    03  -COPY WDD311                                                     
022900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD901'.                      
023000 01  DLI-IO-WDD901.                                                       
023100*    03  -COPY WDD901                                                     
023200     EJECT                                                                
023300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD902'.                      
023400 01  DLI-IO-WDD902.                                                       
023500*    03  -COPY WDD902                                                     
023600     EJECT                                                                
023700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
023800 01  DLI-IO-WDB601.                                                       
023900*    03  -COPY WDB601                                                     
024000     EJECT                                                                
024100 01  FILLER         PIC X(16) VALUE 'DLI-IO-W6D111'.                      
024200 01  DLI-IO-W6D111.                                                       
024300*    03  -COPY W6D111                                                     
024400     EJECT                                                                
024500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4542'.                    
024600 01  DLI-IO-WDGX4542.                                                     
024700*    03  -COPY WDGX4542                                                   
024800     EJECT                                                                
024900 LINKAGE SECTION.                                                         
025000*01  -COPY W0009   -PRE MSG-                                              
025100*01  -COPY W0008   -PRE USEA-                                             
025200     05  FILLER                  PIC X.                                   
025300                                                                          
025400*01  -COPY W0008  -PRE WDK6-                                              
025500     05  FILLER                  PIC X.                                   
025600                                                                          
025700*01  -COPY W0008  -PRE WDK7-                                              
025800     05  FILLER                  PIC X.                                   
025900                                                                          
026000*01  -COPY W0008  -PRE WDK7U-                                             
026100     05  FILLER                  PIC X.                                   
026200                                                                          
026300*01  -COPY W0008  -PRE WDL7-                                              
026400     05  FILLER                  PIC X.                                   
026500                                                                          
026600*01  -COPY W0008  -PRE WDD3-                                              
026700     05  FILLER                  PIC X.                                   
026800                                                                          
026900*01  -COPY W0008  -PRE WDD9-                                              
027000     05  FILLER                  PIC X.                                   
027100                                                                          
027200*01  -COPY W0008  -PRE WDB6-                                              
027300     05  FILLER                  PIC X.                                   
027400                                                                          
027500*01  -COPY W0008  -PRE W6D1-                                              
027600     05  FILLER                  PIC X.                                   
027700     EJECT                                                                
027800                                                                          
027900*01  -COPY W0008  -PRE 4541-                                              
028000     05  FILLER                  PIC X.                                   
028100     EJECT                                                                
028200                                                                          
028300*01  -COPY W0008  -PRE REFL-2501-                                         
028400     05  FILLER                  PIC X.                                   
028500     EJECT                                                                
028600                                                                          
028700*01  -COPY W0008  -PRE REFL-WDB6-                                         
028800     05  FILLER                  PIC X.                                   
028900     EJECT                                                                
029000                                                                          
029100*01  -COPY W0008  -PRE REFL-WDK7-                                         
029200     05  FILLER                  PIC X.                                   
029300     EJECT                                                                
029400 01  REFL-UTIL-WDK6-PCB          PIC X.                                   
029500 01  REFL-UTIL-WDK7-PCB          PIC X.                                   
029600 01  REFL-UTIL-WDB6-PCB          PIC X.                                   
029700     EJECT                                                                
029800                                                                          
029900*****W271UTUP**********                                                   
030000 01  UTUP1-WDK7-PCB              PIC X.                                   
030100 01  UTUP1-WDB6-PCB              PIC X.                                   
030200 01  UTUP1-UTIL-WDK6-PCB         PIC X.                                   
030300 01  UTUP1-UTIL-WDK7-PCB         PIC X.                                   
030400 01  UTUP1-UTIL-WDB6-PCB         PIC X.                                   
030500     EJECT                                                                
030600 PROCEDURE DIVISION  USING MSG-PCB   USEA-PCB WDK6-PCB WDK7-PCB           
030700                           WDK7U-PCB WDL7-PCB WDD3-PCB WDD9-PCB           
030800                           WDB6-PCB  W6D1-PCB 4541-PCB                    
030900                           REFL-2501-PCB REFL-WDB6-PCB                    
031000                           REFL-WDK7-PCB                                  
031100                           REFL-UTIL-WDK6-PCB                             
031200                           REFL-UTIL-WDK7-PCB                             
031300                           REFL-UTIL-WDB6-PCB                             
031400                           UTUP1-WDK7-PCB                                 
031500                           UTUP1-WDB6-PCB                                 
031600                           UTUP1-UTIL-WDK6-PCB                            
031700                           UTUP1-UTIL-WDK7-PCB                            
031800                           UTUP1-UTIL-WDB6-PCB.                           
031900 MAIN SECTION.                                                            
032000     ENTRY 'DLITCBL' USING MSG-PCB   USEA-PCB WDK6-PCB WDK7-PCB           
032100                           WDK7U-PCB WDL7-PCB WDD3-PCB WDD9-PCB           
032200                           WDB6-PCB  W6D1-PCB 4541-PCB                    
032300                           REFL-2501-PCB REFL-WDB6-PCB                    
032400                           REFL-WDK7-PCB                                  
032500                           REFL-UTIL-WDK6-PCB                             
032600                           REFL-UTIL-WDK7-PCB                             
032700                           REFL-UTIL-WDB6-PCB                             
032800                           UTUP1-WDK7-PCB                                 
032900                           UTUP1-WDB6-PCB                                 
033000                           UTUP1-UTIL-WDK6-PCB                            
033100                           UTUP1-UTIL-WDK7-PCB                            
033200                           UTUP1-UTIL-WDB6-PCB.                           
033300                                                                          
033400     PERFORM IMS-GET-MSG                                                  
033500     IF SEGMENT-FINNS                                                     
033600       PERFORM A-INIT                                                     
033700       PERFORM B-KOLLA-NYCKLAR                                            
033800       IF NYCKLAR-OK                                                      
033900         PERFORM F-LAES-VISA-INFO                                         
034000*---                                                                      
034100*---     SAVE MFG SUPPLIER USING INIT-IO-AREA                             
034200         IF IDLEVNR-WS > SPACES                                           
034300            MOVE ALL '+'           TO MSGI-WMSGINIT                       
034400            MOVE '001'             TO MSGI-KDCALL                         
034500            MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                   
034600            MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                         
034700            MOVE '2402'            TO MSGI-IDTRANS                        
034800            MOVE IDLEVNR-WS        TO MSGI-IDLEVNR                        
034900                                                                          
035000            CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                    
035100         END-IF                                                           
035200       END-IF                                                             
035300       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O40201 + 4                      
035400       PERFORM IMS-INSERT-MSG                                             
035500     END-IF                                                               
035600                                                                          
035700     MOVE ZERO TO RETURN-CODE                                             
035800     GOBACK                                                               
035900     .                                                                    
036000     EJECT                                                                
036100 A-INIT SECTION.                                                          
036200     MOVE 'A-INIT          '  TO CURRENT-SECTION                          
036300                                                                          
036400     IF MSG-DUBBLA-TRANSKODER                                             
036500       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I40201                 
036600       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
036700       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
036800     ELSE                                                                 
036900       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I40201                  
037000       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
037100       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
037200     END-IF                                                               
037300                                                                          
037400     MOVE MSG-KDTRTYP      TO MFS-KDTRTYP                                 
037500     MOVE MSG-IDPFK        TO MFS-IDPFK                                   
037600     MOVE MFS-IDTRANS      TO W-IDTRANS                                   
037700                                                                          
037800     MOVE LOW-VALUE        TO MSG-AREA                                    
037900     MOVE 'W2O402N1'       TO MFS-IDMOD                                   
038000     MOVE '2402'           TO MOD-IDTRANS                                 
038100     MOVE MFS-RENSA-FAELT  TO MOD-TEMFSFEL MOD-TEMFSINF                   
038200                                                                          
038300     IF EGEN-MID OR HELP-MID                                              
038400       CONTINUE                                                           
038500     ELSE                                                                 
038600       MOVE SPACE TO MFS-KDTRTYP                                          
038700       MOVE '7'   TO MFS-IDPFK                                            
038800     END-IF                                                               
038900                                                                          
039000     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-TODAYS-AAAAMMDD               
039100                                                                          
039200     MOVE 1       TO WS-IDDC-REF-IX                                       
039300     PERFORM UNTIL WS-IDDC-REF-IX > WS-IDDC-REF-IX-MAX                    
039400        MOVE SPACE TO WS-IDDC-REF(WS-IDDC-REF-IX)                         
039500        ADD 1      TO WS-IDDC-REF-IX                                      
039600     END-PERFORM                                                          
039700     .                                                                    
039800     EJECT                                                                
039900 B-KOLLA-NYCKLAR SECTION.                                                 
040000     MOVE 'B-KOLLA-NYCKLAR '  TO CURRENT-SECTION                          
040100                                                                          
040200     MOVE ALL '+'             TO MSGI-WMSGINIT                            
040300     MOVE '001'               TO MSGI-KDCALL                              
040400     MOVE MSG-LTERM-NAME      TO MSGI-IDLTERM-USER                        
040500     MOVE MSG-SIGNON-USERID   TO MSGI-IDUSER                              
040600     MOVE '2402'              TO MSGI-IDTRANS                             
040700     IF EGEN-MID OR 2471-MID                                              
040800        MOVE MID-IDARTNR-IN   TO MSGI-IDARTNR                             
040900        MOVE MID-IDDC-IN      TO MSGI-IDDC-KEY                            
041000     END-IF                                                               
041100     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
041200     MOVE MSGI-SPAR-AREA      TO SPAR-AREA                                
041300                                                                          
041400*    - SPRÅK SOM SKA ANVÄNDAS AV WMEDKONV                                 
041500*    MOVE MSGI-IDLAND-SPR     TO MED-IDSKYLT                              
041600     MOVE 'GB'                TO MED-IDSKYLT                              
041700                                                                          
041800     MOVE JA    TO NYCKLAR-SW                                             
041900     MOVE SPACE TO MED-IDMFSFEL                                           
042000                                                                          
042100*    -- KONTROLL AV IDARTNR                                               
042200     MOVE MFS-RENSA-FAELT     TO MOD-IDARTNR-IN                           
042300                                                                          
042400                                                                          
042500     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
042600     IF MSGI-IDARTNR NUMERIC                                              
042700        MOVE MSGI-IDARTNR    TO W-IDARTNR                                 
042800        PERFORM IMS-GU-WDK601                                             
042900        IF SEGMENT-SAKNAS                                                 
043000           MOVE ERR-ARTIKEL-SAKNAS                                        
043100                             TO MED-IDMFSFEL                              
043200           MOVE NEJ          TO NYCKLAR-SW                                
043300        END-IF                                                            
043400     ELSE                                                                 
043500        MOVE NEJ TO NYCKLAR-SW                                            
043600     END-IF                                                               
043700                                                                          
043800*    -- KONTROLL AV IDDC                                                  
043900     IF NYCKLAR-OK                                                        
044000       MOVE MFS-RENSA-FAELT   TO MOD-IDDC-IN                              
044100                                                                          
044200       IF MID-IDDC-IN NOT = ALL '+'                                       
044300         MOVE '7'             TO MFS-IDPFK                                
044400         MOVE SPACE           TO MFS-KDTRTYP                              
044500       END-IF                                                             
044600       MOVE MSGI-IDDC-KEY     TO W-IDDC                                   
044700                                 W-IDDC-B6                                
044800       PERFORM IMS-GU-WDB601                                              
044900       IF SEGMENT-SAKNAS                                                  
045000          MOVE ERR-DC-SAKNAS                                              
045100                              TO MED-IDMFSFEL                             
045200          MOVE NEJ            TO NYCKLAR-SW                               
045300       ELSE                                                               
045400          IF DCS-NDC-CN                                                   
045500          OR (DCS-NDC-NA AND DCS-USA)                                     
045600             MOVE DCS-IDLANDX2 TO WS-IDLANDX2                             
045700          ELSE                                                            
045800             MOVE NEJ         TO NYCKLAR-SW                               
045900          END-IF                                                          
046000       END-IF                                                             
046100     END-IF                                                               
046200                                                                          
046300     IF GODK-MID OR NYCKLAR-OK                                            
046400       MOVE MSGI-IDARTNR      TO MOD-IDARTNR-UT                           
046500       MOVE MSGI-IDDC-KEY     TO MOD-IDDC-UT                              
046600       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
046700     ELSE                                                                 
046800       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                             
046900       MOVE MFS-RENSA-FAELT TO MOD-IDDC-UT                                
047000     END-IF                                                               
047100                                                                          
047200     IF NYCKLAR-FEL                                                       
047300       IF MED-IDMFSFEL = SPACE                                            
047400         MOVE ERR-WRONG-KEY   TO MED-IDMFSFEL                             
047500       END-IF                                                             
047600       CALL WMEDKONV USING MED-WMEDAREA                                   
047700       MOVE MED-MFSFEL        TO MOD-TEMFSFEL                             
047800       PERFORM MFS-RENSA-FAELT-IN                                         
047900       PERFORM MFS-RENSA-FAELT-UT                                         
048000     END-IF                                                               
048100     .                                                                    
048200     EJECT                                                                
048300 F-LAES-VISA-INFO SECTION.                                                
048400     MOVE 'F-LAES-VISA-INFO'  TO CURRENT-SECTION                          
048500                                                                          
048600     MOVE '-'            TO MOD-STRECK-1                                  
048700     MOVE ART-REKSIFFR   TO MOD-REKSIFFR                                  
048800     MOVE ART-TIURPROD   TO MOD-TIURPROD                                  
048800     MOVE ART-KDANSKSEG  TO MOD-KDANSKSEG                                 
048900                                                                          
049000     PERFORM FA-VISA-D311-INFO                                            
049100     PERFORM FB-VISA-K611-INFO                                            
049200     PERFORM FC-VISA-L711-INFO                                            
049300     PERFORM FD-VISA-D902-INFO                                            
049400                                                                          
049500     PERFORM FE-VISA-K711-INFO                                            
049600     PERFORM FF-VISA-K712-INFO                                            
049700                                                                          
049800     PERFORM FG-VISA-K722-INFO                                            
049900                                                                          
050000     PERFORM FI-SUMMERA-K711-K722                                         
050100     PERFORM FJ-VISA-W6D1-INFO                                            
050200     .                                                                    
050300     EJECT                                                                
050400 FA-VISA-D311-INFO SECTION.                                               
050500     MOVE 'FA-VISA-D311-INF'  TO CURRENT-SECTION                          
050600                                                                          
050700     MOVE 'GB' TO W-IDSKYLT                                               
050800     PERFORM IMS-GU-WDD311                                                
050900     IF SEGMENT-FINNS                                                     
051000        MOVE TEXT-BEART TO MOD-BEART-ENG                                  
051100     ELSE                                                                 
051200        MOVE 'UNKNOWN'  TO MOD-BEART-ENG                                  
051300     END-IF                                                               
051400     .                                                                    
051500     EJECT                                                                
051600 FB-VISA-K611-INFO SECTION.                                               
051700     MOVE 'FB-VISA-K611-INF'  TO CURRENT-SECTION                          
051800                                                                          
051900     PERFORM IMS-GU-WDK711                                                
052000     IF SEGMENT-FINNS                                                     
052100* -- CDC-INFO BARA ARTIKELN FINNS PÅ GIVET LAGER                          
052200        PERFORM IMS-GU-WDK611                                             
052300        IF SEGMENT-FINNS                                                  
052400           MOVE CLAG-KDERS TO WS-KDERS                                    
052500           MOVE WS-KDERS   TO MOD-KDERS                                   
052600        END-IF                                                            
052700     END-IF                                                               
052800     .                                                                    
052900     EJECT                                                                
053000 FC-VISA-L711-INFO SECTION.                                               
053100     MOVE 'FC-VISA-L711-INF'  TO CURRENT-SECTION                          
053200                                                                          
053300     PERFORM IMS-GU-WDL711                                                
053400     IF SEGMENT-FINNS                                                     
053500        MOVE DC-TIREFEFT   TO MOD-TIREFEFT                                
053600     END-IF                                                               
053700     .                                                                    
053800     EJECT                                                                
053900 FD-VISA-D902-INFO SECTION.                                               
054000     MOVE 'FD-VISA-D902-INF'  TO CURRENT-SECTION                          
054100                                                                          
054200     MOVE SLAG-IDLEVNR     TO W-IDLEVNR                                   
054300     PERFORM IMS-GU-WDD902                                                
054400     IF SEGMENT-FINNS                                                     
054500        MOVE TILEVPL       TO MOD-TILEVPL                                 
054600     END-IF                                                               
054700     .                                                                    
054800     EJECT                                                                
054900 FE-VISA-K711-INFO SECTION.                                               
055000     MOVE 'FE-VISA-K711-INF'  TO CURRENT-SECTION                          
055100                                                                          
055200     MOVE NOO                  TO WS-WDK711-EXIST                         
055300     PERFORM IMS-GU-WDK711                                                
055400     IF SEGMENT-FINNS                                                     
055500        MOVE YES               TO WS-WDK711-EXIST                         
055600        MOVE SLAG-IDLEVNR      TO MOD-IDLEVNR-AVT                         
055700                                  IDLEVNR-WS                              
055800        MOVE SLAG-IDREFTAB     TO MOD-IDREFTAB                            
055900        MOVE SLAG-ADLAGOMR     TO MOD-ADLAGOMR                            
056000        MOVE SLAG-ADGANG       TO MOD-ADGANG                              
056100        MOVE SLAG-ADPLATS      TO MOD-ADPLATS                             
056200        MOVE SLAG-KVLS         TO MOD-KVLS(1)                             
056300        MOVE SLAG-KVRESS       TO MOD-KVRESS(1)                           
056400        COMPUTE MOD-STONHND(1) =                                          
056500                SLAG-KVLS - SLAG-KVRESS                                   
056600        MOVE SLAG-KVPB-REF     TO MOD-KVPB-REF(1)                         
056700        MOVE SLAG-TIREFMPB     TO MOD-TIREFMPB                            
056800        COMPUTE MOD-KVOKS(1) =                                            
056900                SLAG-KVOKS-BULK + SLAG-KVOKS-DAG                          
057000        MOVE SLAG-KVAKS-SDC    TO MOD-KVAKS-SDC(1)                        
057100        MOVE SLAG-KVREFBER     TO MOD-KVREFBER                            
057200        MOVE SLAG-TIREFPAF     TO MOD-TIREFPAF                            
057300        MOVE SLAG-KVAKS-PAV    TO MOD-KVAKS-PAV(1)                        
057400        MOVE SLAG-TIINVDAT     TO MOD-TIINVDAT                            
057500        COMPUTE MOD-KVROS(1) =                                            
057600             SLAG-KVROS-BULK + SLAG-KVROS-DAG                             
057700                                                                          
057800        MOVE +0                TO W-SUM-KVBEART-Q                         
057900        PERFORM IMS-GU-WDGX4541                                           
058000        IF SEGMENT-FINNS                                                  
058100          PERFORM IMS-GNP-WDGX4542                                        
058200          PERFORM UNTIL SEGMENT-SAKNAS                                    
058300            IF SLAG-IDDC-REF = SPACE                                      
058400               ADD 4542-KVBEART-Q TO W-SUM-KVBEART-Q                      
058500            END-IF                                                        
058600            PERFORM IMS-GNP-WDGX4542                                      
058700          END-PERFORM                                                     
058800        END-IF                                                            
058900        MOVE W-SUM-KVBEART-Q   TO MOD-KVBEART-Q                           
059000        MOVE SLAG-KVUTRS       TO MOD-KVUTRS(1)                           
059100        MOVE SLAG-KVEFRS       TO MOD-KVEFRS(1)                           
059200        COMPUTE WS-KVREFOVL-CURRENT =                                     
059300                SLAG-KVLS - SLAG-KVRESS - SLAG-KVREFOVL                   
059400                                                                          
059500     END-IF                                                               
059600     .                                                                    
059700     EJECT                                                                
059800 FF-VISA-K712-INFO SECTION.                                               
059900     MOVE 'FF-VISA-K712-INF'  TO CURRENT-SECTION                          
060000                                                                          
060100     MOVE WS-IDLANDX2          TO W-IDLAND-X                              
060200     PERFORM IMS-GU-WDK712                                                
060300     IF SEGMENT-FINNS                                                     
060400        MOVE LART-PRMATRL     TO MOD-PRMATRL                              
060500        IF LART-KDMATRPR = 2                                              
060600           MOVE YES           TO MOD-KDMATRPR                             
060700        ELSE                                                              
060800           MOVE NOO           TO MOD-KDMATRPR                             
060900        END-IF                                                            
061000                                                                          
061100        IF LART-TIERSDAT-VIPS > ZERO                                      
061200           MOVE YES           TO MOD-FLTOVIPS                             
061300        ELSE                                                              
061400           MOVE NOO           TO MOD-FLTOVIPS                             
061500        END-IF                                                            
061600        IF WS-WDK711-EXIST = YES                                          
061700           PERFORM FH-READ-SHOW-W271REFL                                  
061800        END-IF                                                            
061900     END-IF                                                               
062000     .                                                                    
062100     EJECT                                                                
062200 FG-VISA-K722-INFO SECTION.                                               
062300     MOVE 'FG-VISA-K722-INF'  TO CURRENT-SECTION                          
062400                                                                          
062500     MOVE W-IDDC TO W-IDDC-K7                                             
062600     PERFORM IMS-GU-WDK722                                                
062700     IF SEGMENT-FINNS                                                     
062800        MOVE XLAG-IDLEVNR-SHIP TO MOD-IDLEVNR-SHIP                        
062900        MOVE XLAG-IDANSK       TO MOD-IDANSK                              
063000        MOVE XLAG-KDAVT        TO MOD-KDAVT                               
063100        MOVE XLAG-IDPLANGR-AG  TO MOD-IDPLANGR-AG                         
063200        MOVE XLAG-KVSLAGER     TO MOD-KVSLAGER                            
063300        MOVE XLAG-TIMANSEC     TO MOD-TIMANSEC                            
063400        MOVE XLAG-KVSPANT      TO MOD-KVSPANT(1)                          
063500     END-IF                                                               
063600     .                                                                    
063700     EJECT                                                                
063800 FH-READ-SHOW-W271REFL SECTION.                                           
063900                                                                          
064000     INITIALIZE REFL-W271REFL                                             
064100                                                                          
064200     MOVE W-IDDC                     TO REFL-IDDC                         
064300     MOVE W-IDARTNR                  TO REFL-IDARTNR                      
064400     MOVE SLAG-IDDC-REF              TO REFL-IDDC-REF                     
064500     MOVE SLAG-IDREFTAB              TO REFL-IDREFTAB                     
064600     MOVE LART-PRMATRL               TO REFL-PRARTBES                     
064700                                                                          
064800     INITIALIZE UTUP-W271UTUP                                             
064900     MOVE 004                   TO UTUP-KDCALL                            
065000     MOVE W-IDARTNR             TO UTUP-IDARTNR                           
065100     MOVE W-IDDC                TO UTUP-IDDC                              
065200     MOVE SLAG-IDDC-REF         TO UTUP-IDDC-REF                          
065300                                                                          
065400     CALL W271UTUP USING UTUP-W271UTUP                                    
065500                         UTUP1-WDK7-PCB                                   
065600                         UTUP1-WDB6-PCB                                   
065700                         UTUP1-UTIL-WDK6-PCB                              
065800                         UTUP1-UTIL-WDK7-PCB                              
065900                         UTUP1-UTIL-WDB6-PCB                              
066000     IF UTUP-KDSVAR-OK                                                    
066100        MOVE UTUP-LEADTID-BEHOV      TO REFL-IN-LEADTID-BEHOV             
066200     ELSE                                                                 
066300        DISPLAY 'W271UTUP-ERROR :' UTUP-TEXT                              
066400        CALL FELLOG                                                       
066500     END-IF                                                               
066600                                                                          
066700     CALL W271REFL USING REFL-W271REFL REFL-2501-PCB                      
066800                                       REFL-WDB6-PCB                      
066900                                       REFL-WDK7-PCB                      
067000                                       REFL-UTIL-WDK6-PCB                 
067100                                       REFL-UTIL-WDK7-PCB                 
067200                                       REFL-UTIL-WDB6-PCB                 
067300     MOVE REFL-KLASS (2:1)           TO MOD-KDPRISKL                      
067400     MOVE REFL-KLASS (3:1)           TO MOD-KDFREKKL                      
067500     .                                                                    
067600     EJECT                                                                
067700 FI-SUMMERA-K711-K722  SECTION.                                           
067800     MOVE 'FI-SUM-K711-K722'  TO CURRENT-SECTION                          
067900                                                                          
068000     MOVE ZERO TO WS-KVRESS-SUM                                           
068100                  WS-STONHND-SUM                                          
068200                  WS-KVPB-SUM                                             
068300                  WS-KVLS-SUM                                             
068400                  WS-KVOKS-SUM                                            
068500                  WS-KVAKS-SDC-SUM                                        
068600                  WS-KVAKS-PAV-SUM                                        
068700                  WS-KVROS-SUM                                            
068800                  WS-KVUTRS-SUM                                           
068900                  WS-KVEFRS-SUM                                           
069000                  WS-KVSPANT-SUM                                          
069100                  WS-KVREFOVL-SUM                                         
069200                                                                          
069300     MOVE 1    TO WS-IDDC-REF-IX                                          
069400                                                                          
069500     PERFORM IMS-GU-WDK701                                                
069600     IF SEGMENT-FINNS                                                     
069700        PERFORM IMS-GNP-WDK711                                            
069800        PERFORM UNTIL SEGMENT-SAKNAS                                      
069900           ADD SLAG-KVRESS   TO  WS-KVRESS-SUM                            
070000           COMPUTE WS-STONHND-SUM =                                       
070100                   WS-STONHND-SUM + SLAG-KVLS - SLAG-KVRESS               
070200           ADD SLAG-KVPB-REF TO  WS-KVPB-SUM                              
070300           ADD SLAG-KVPBREOI TO  WS-KVPB-SUM                              
070400           ADD SLAG-KVLS     TO  WS-KVLS-SUM                              
070500           COMPUTE WS-KVOKS-SUM =                                         
070600                   WS-KVOKS-SUM + SLAG-KVOKS-BULK + SLAG-KVOKS-DAG        
070700           ADD SLAG-KVAKS-SDC TO WS-KVAKS-SDC-SUM                         
070800           ADD SLAG-KVAKS-PAV TO WS-KVAKS-PAV-SUM                         
070900           COMPUTE WS-KVROS-SUM =                                         
071000                   WS-KVROS-SUM + SLAG-KVROS-BULK + SLAG-KVROS-DAG        
071100           ADD SLAG-KVUTRS   TO  WS-KVUTRS-SUM                            
071200           ADD SLAG-KVEFRS   TO  WS-KVEFRS-SUM                            
071300                                                                          
071400           MOVE SLAG-IDDC    TO  W-IDDC-B6                                
071500           PERFORM IMS-GU-WDB601                                          
071600           IF SEGMENT-FINNS AND (DCS-FLOVRLAGBER = JA OR YES)             
071700              COMPUTE WS-KVREFOVL-CURRENT =                               
071800                     (SLAG-KVLS - SLAG-KVRESS                             
071900                    - SLAG-KVOKS-DAG - SLAG-KVOKS-BULK                    
072000                    - SLAG-KVREFOVL)                                      
072100              IF WS-KVREFOVL-CURRENT > ZERO                               
072200                 COMPUTE WS-KVREFOVL-SUM =                                
072300                         WS-KVREFOVL-SUM + WS-KVREFOVL-CURRENT            
072400              END-IF                                                      
072500           END-IF                                                         
072600                                                                          
072700           MOVE SLAG-IDDC TO W-IDDC-K7                                    
072800           PERFORM IMS-GU-WDK722                                          
072900           IF SEGMENT-FINNS                                               
073000              ADD XLAG-KVSPANT TO WS-KVSPANT-SUM                          
073100           END-IF                                                         
073200           PERFORM FIA-SPARA-IDDC-REF                                     
073300           MOVE W-IDDC TO W-IDDC-K7                                       
073400           PERFORM IMS-GNP-WDK711                                         
073500        END-PERFORM                                                       
073600     END-IF                                                               
073700                                                                          
073800     PERFORM IMS-GU-WDK611                                                
073900     IF SEGMENT-FINNS                                                     
074000       IF CLAG-IDDC-REF = W-IDDC                                          
074100         PERFORM IMS-GNP-WDK629                                           
074200         IF SEGMENT-FINNS                                                 
074300           IF CLAG-DAPBPLAN >= WS-TODAYS-AAAAMMDD                         
074400              ADD CLAG-KVPB-PLAN   TO WS-KVPB-SUM                         
074500           ELSE                                                           
074600              ADD CREF-KVPB-PLAN   TO WS-KVPB-SUM                         
074700           END-IF                                                         
074800         END-IF                                                           
074900       END-IF                                                             
075000     END-IF                                                               
075100                                                                          
075200     MOVE WS-KVRESS-SUM    TO MOD-KVRESS(2)                               
075300     MOVE WS-STONHND-SUM   TO MOD-STONHND(2)                              
075400     MOVE WS-KVPB-SUM      TO MOD-KVPB-REF(2)                             
075500     MOVE WS-KVLS-SUM      TO MOD-KVLS(2)                                 
075600     MOVE WS-KVOKS-SUM     TO MOD-KVOKS(2)                                
075700     MOVE WS-KVAKS-SDC-SUM TO MOD-KVAKS-SDC(2)                            
075800     MOVE WS-KVAKS-PAV-SUM TO MOD-KVAKS-PAV(2)                            
075900     MOVE WS-KVROS-SUM     TO MOD-KVROS(2)                                
076000     MOVE WS-KVUTRS-SUM    TO MOD-KVUTRS(2)                               
076100     MOVE WS-KVEFRS-SUM    TO MOD-KVEFRS(2)                               
076200     MOVE WS-KVSPANT-SUM   TO MOD-KVSPANT(2)                              
076300     MOVE WS-KVREFOVL-SUM  TO MOD-KVREFOVL                                
076400     .                                                                    
076500                                                                          
076600     EJECT                                                                
076700 FIA-SPARA-IDDC-REF SECTION.                                              
076800     MOVE 'FIA-SPARA-IDDC-R'  TO CURRENT-SECTION                          
076900                                                                          
077000*    SPARA IDDC-REF FÖR SENARE LÄSNING AV W6D1                            
077100                                                                          
077200     IF WS-IDDC-REF-IX > WS-IDDC-REF-IX-MAX                               
077300        MOVE 'IDDC-REF-TEBELLEN FULL' TO FELTEXT                          
077400        CALL FELLOG                                                       
077500     END-IF                                                               
077600                                                                          
077700     IF  SLAG-IDDC-REF = W-IDDC                                           
077800     MOVE SLAG-IDDC TO WS-IDDC-REF(WS-IDDC-REF-IX)                        
077900     ADD  1         TO WS-IDDC-REF-IX                                     
078000     END-IF                                                               
078100     .                                                                    
078200                                                                          
078300     EJECT                                                                
078400 FJ-VISA-W6D1-INFO  SECTION.                                              
078500     MOVE 'FJ-VISA-W6D1-INF'  TO CURRENT-SECTION                          
078600                                                                          
078700     MOVE ZERO      TO WS-KVAVIS                                          
078800                                                                          
078900     MOVE W-IDARTNR TO W-IDARTNR-HSEQ                                     
079000     PERFORM IMS-GN-W6D111-HSEQ                                           
079100     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
079200        IF ART-IDDC     = W-IDDC                                          
079300       AND ART-IDLOPNRM = ZERO                                            
079400       AND ART-FLFEL    = NEJ                                             
079500           ADD ART-KVAVIS     TO WS-KVAVIS                                
079600        END-IF                                                            
079700        PERFORM IMS-GN-W6D111-HSEQ                                        
079800     END-PERFORM                                                          
079900                                                                          
080000     MOVE WS-KVAVIS           TO MOD-KVAVIS                               
080100     .                                                                    
080200                                                                          
080300     EJECT                                                                
080400 MFS-RENSA-FAELT-UT SECTION.                                              
080500                                                                          
080600*    --- ALLA UTDATA-FÄLT                                                 
080700     MOVE MFS-RENSA-FAELT TO MOD-IDANSK                                   
080800                             MOD-IDLEVNR-SHIP                             
080900                             MOD-KDAVT                                    
081000                             MOD-PRMATRL                                  
081100                             MOD-KDMATRPR                                 
081200                             MOD-IDPLANGR-AG                              
081300                             MOD-IDLEVNR-AVT                              
081400                             MOD-KDFREKKL                                 
081500                             MOD-KDPRISKL                                 
081600                             MOD-IDREFTAB                                 
081700                             MOD-ADLAGOMR                                 
081800                             MOD-ADGANG                                   
081900                             MOD-ADPLATS                                  
082000                             MOD-KVLS(1)                                  
082100                             MOD-KVLS(2)                                  
082200                             MOD-KVRESS(1)                                
082300                             MOD-KVRESS(2)                                
082400                             MOD-STONHND(1)                               
082500                             MOD-STONHND(2)                               
082600                             MOD-KVOKS(1)                                 
082700                             MOD-KVOKS(2)                                 
082800                             MOD-KVAKS-SDC(1)                             
082900                             MOD-KVAKS-SDC(2)                             
083000                             MOD-KVAKS-PAV(1)                             
083100                             MOD-KVAKS-PAV(2)                             
083200                             MOD-KVAVIS                                   
083300                             MOD-KVBEART-Q                                
083400                             MOD-KVROS(1)                                 
083500                             MOD-KVROS(2)                                 
083600                             MOD-KDERS                                    
083700                             MOD-FLTOVIPS                                 
083800                             MOD-TIURPROD                                 
083900                             MOD-TILEVPL                                  
084000                             MOD-TIREFEFT                                 
084100                             MOD-KVPB-REF(1)                              
084200                             MOD-KVPB-REF(2)                              
084300                             MOD-TIREFMPB                                 
084400                             MOD-KVSLAGER                                 
084500                             MOD-TIMANSEC                                 
084600                             MOD-KVREFBER                                 
084700                             MOD-TIREFPAF                                 
084800                             MOD-TIINVDAT                                 
084900                             MOD-KVUTRS(1)                                
085000                             MOD-KVUTRS(2)                                
085100                             MOD-KVEFRS(1)                                
085200                             MOD-KVEFRS(2)                                
085300                             MOD-KVSPANT(1)                               
085400                             MOD-KVSPANT(2)                               
085500                             MOD-KVREFOVL                                 
085600     .                                                                    
085700     SKIP3                                                                
085800 MFS-RENSA-FAELT-IN SECTION.                                              
085900                                                                          
086000*    --- ALLA INDATA-FÄLT                                                 
086100     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
086200                             MOD-IDDC-IN                                  
086300     .                                                                    
086400     EJECT                                                                
086500 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
086600                                                                          
086700*    --- ALLA UTDATA-FÄLT                                                 
086800     MOVE MFS-ROER-EJ-FAELT TO MOD-IDANSK                                 
086900                               MOD-IDLEVNR-SHIP                           
087000                               MOD-KDAVT                                  
087100                               MOD-PRMATRL                                
087200                               MOD-KDMATRPR                               
087300                               MOD-IDPLANGR-AG                            
087400                               MOD-IDLEVNR-AVT                            
087500                               MOD-KDFREKKL                               
087600                               MOD-KDPRISKL                               
087700                               MOD-IDREFTAB                               
087800                               MOD-ADLAGOMR                               
087900                               MOD-ADGANG                                 
088000                               MOD-ADPLATS                                
088100                               MOD-KVLS(1)                                
088200                               MOD-KVLS(2)                                
088300                               MOD-KVRESS(1)                              
088400                               MOD-KVRESS(2)                              
088500                               MOD-STONHND(1)                             
088600                               MOD-STONHND(2)                             
088700                               MOD-KVOKS(1)                               
088800                               MOD-KVOKS(2)                               
088900                               MOD-KVAKS-SDC(1)                           
089000                               MOD-KVAKS-SDC(2)                           
089100                               MOD-KVAKS-PAV(1)                           
089200                               MOD-KVAKS-PAV(2)                           
089300                               MOD-KVAVIS                                 
089400                               MOD-KVBEART-Q                              
089500                               MOD-KVROS(1)                               
089600                               MOD-KVROS(2)                               
089700                               MOD-KDERS                                  
089800                               MOD-FLTOVIPS                               
089900                               MOD-TIURPROD                               
090000                               MOD-TILEVPL                                
090100                               MOD-TIREFEFT                               
090200                               MOD-KVPB-REF(1)                            
090300                               MOD-KVPB-REF(2)                            
090400                               MOD-TIREFMPB                               
090500                               MOD-KVSLAGER                               
090600                               MOD-TIMANSEC                               
090700                               MOD-KVREFBER                               
090800                               MOD-TIREFPAF                               
090900                               MOD-TIINVDAT                               
091000                               MOD-KVUTRS(1)                              
091100                               MOD-KVUTRS(2)                              
091200                               MOD-KVEFRS(1)                              
091300                               MOD-KVEFRS(2)                              
091400                               MOD-KVSPANT(1)                             
091500                               MOD-KVSPANT(2)                             
091600                               MOD-KVREFOVL                               
091700     .                                                                    
091800     SKIP3                                                                
091900 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
092000                                                                          
092100*    --- ALLA INDATA-FÄLT                                                 
092200     MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-IN                             
092300                               MOD-IDDC-IN                                
092400     .                                                                    
092500     EJECT                                                                
092600*MFS-FORM-ATTR SECTION.                                                   
092700*                                                                         
092800*    --- ALLA INDATA-FÄLT                                                 
092900*    MOVE MFS-FORMATETS-ATTR TO MOD-XXXXXXXX-ATTR                         
093000*                               MOD-XXXXXXXX-ATTR                         
093100*    .                                                                    
093200     SKIP2                                                                
093300*MFS-LAES-IN-IGEN SECTION.                                                
093400                                                                          
093500*    --- ALLA INDATA-FÄLT                                                 
093600*    MOVE MFS-ADD-LAES-IN-FAELT TO MOD-XXXXXXXX-ATTR                      
093700*                                  MOD-XXXXXXXX-ATTR                      
093800*    .                                                                    
093900*    EJECT                                                                
094000* --- IMS SEKTIONER ---                                                   
094100     SKIP3                                                                
094200 IMS-GET-MSG SECTION.                                                     
094300                                                                          
094400     MOVE '  QC' TO GODK-STATUSKODER                                      
094500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
094600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
094700     PERFORM IMS-STATUSKONTROLL                                           
094800     .                                                                    
094900     SKIP3                                                                
095000 IMS-INSERT-MSG SECTION.                                                  
095100                                                                          
095200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
095300     MOVE SPACE TO GODK-STATUSKODER                                       
095400     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
095500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
095600     PERFORM IMS-STATUSKONTROLL                                           
095700     .                                                                    
095800     EJECT                                                                
095900 IMS-GU-WDK601 SECTION.                                                   
096000     MOVE 'IMS-GU-WDK601   '  TO CURRENT-IMS-SECTION                      
096100                                                                          
096200     MOVE SPACE               TO ALL-SSA                                  
096300     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
096400          DELIMITED BY SIZE INTO SSA1                                     
096500     MOVE '  GE' TO GODK-STATUSKODER                                      
096600     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
096700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
096800     PERFORM IMS-STATUSKONTROLL                                           
096900     .                                                                    
097000                                                                          
097100 IMS-GU-WDK611 SECTION.                                                   
097200     MOVE 'IMS-GU-WDK611   '  TO CURRENT-IMS-SECTION                      
097300                                                                          
097400     MOVE SPACE               TO ALL-SSA                                  
097500     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
097600          DELIMITED BY SIZE INTO SSA1                                     
097700     MOVE   'WDK611 '         TO SSA2                                     
097800     MOVE '  GE' TO GODK-STATUSKODER                                      
097900     CALL CBLTDLI USING GU  WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
098000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
098100     PERFORM IMS-STATUSKONTROLL                                           
098200     .                                                                    
098300     EJECT                                                                
098400 IMS-GNP-WDK629 SECTION.                                                  
098500     MOVE 'IMS-GNP-WDK629   ' TO CURRENT-IMS-SECTION                      
098600                                                                          
098700     MOVE SPACE               TO ALL-SSA                                  
098800     MOVE 'WDK629  '          TO SSA1                                     
098900     MOVE '  GE'              TO GODK-STATUSKODER                         
099000     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK629 SSA1                   
099100     MOVE WDK6-STATUS-CODE    TO STATUS-WS                                
099200     PERFORM IMS-STATUSKONTROLL                                           
099300     .                                                                    
099400     EJECT                                                                
099500 IMS-GU-WDK701 SECTION.                                                   
099600     MOVE 'IMS-GU-WDK701   '  TO CURRENT-IMS-SECTION                      
099700                                                                          
099800     MOVE SPACE               TO ALL-SSA                                  
099900     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
100000          DELIMITED BY SIZE INTO SSA1                                     
100100     MOVE '  GE' TO GODK-STATUSKODER                                      
100200     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
100300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
100400     PERFORM IMS-STATUSKONTROLL                                           
100500     .                                                                    
100600     EJECT                                                                
100700 IMS-GU-WDK711 SECTION.                                                   
100800     MOVE 'IMS-GU-WDK711   '  TO CURRENT-IMS-SECTION                      
100900                                                                          
101000     MOVE SPACE               TO ALL-SSA                                  
101100     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
101200          DELIMITED BY SIZE INTO SSA1                                     
101300     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
101400          DELIMITED BY SIZE INTO SSA2                                     
101500     MOVE '  GE' TO GODK-STATUSKODER                                      
101600     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
101700     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
101800     PERFORM IMS-STATUSKONTROLL                                           
101900     .                                                                    
102000     EJECT                                                                
102100 IMS-GNP-WDK711 SECTION.                                                  
102200     MOVE 'IMS-GNP-WDK711  '  TO CURRENT-IMS-SECTION                      
102300                                                                          
102400     MOVE SPACE               TO ALL-SSA                                  
102500     STRING 'WDK711  (IDDCREF  =' W-IDDC-K7-X ')'                         
102600          DELIMITED BY SIZE INTO SSA1                                     
102700     MOVE '  GE' TO GODK-STATUSKODER                                      
102800     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK711 SSA1                   
102900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
103000     PERFORM IMS-STATUSKONTROLL                                           
103100     .                                                                    
103200     EJECT                                                                
103300 IMS-GU-WDK722 SECTION.                                                   
103400     MOVE 'IMS-GU-WDK722   '  TO CURRENT-IMS-SECTION                      
103500                                                                          
103600     MOVE SPACE               TO ALL-SSA                                  
103700     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
103800          DELIMITED BY SIZE INTO SSA1                                     
103900     STRING 'WDK711  (IDDC     =' W-IDDC-K7-X ')'                         
104000          DELIMITED BY SIZE INTO SSA2                                     
104100     MOVE   'WDK722 '         TO SSA3                                     
104200     MOVE '  GE' TO GODK-STATUSKODER                                      
104300     CALL CBLTDLI USING GU WDK7U-PCB DLI-IO-WDK722 SSA1 SSA2 SSA3         
104400     MOVE WDK7U-STATUS-CODE TO STATUS-WS                                  
104500     PERFORM IMS-STATUSKONTROLL                                           
104600     .                                                                    
104700     EJECT                                                                
104800 IMS-GU-WDK712 SECTION.                                                   
104900     MOVE 'IMS-GU-WDK712   '  TO CURRENT-IMS-SECTION                      
105000                                                                          
105100     MOVE SPACE               TO ALL-SSA                                  
105200     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
105300          DELIMITED BY SIZE INTO SSA1                                     
105400     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
105500          DELIMITED BY SIZE INTO SSA2                                     
105600     MOVE '  GE' TO GODK-STATUSKODER                                      
105700     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK712 SSA1 SSA2               
105800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
105900     PERFORM IMS-STATUSKONTROLL                                           
106000     .                                                                    
106100     EJECT                                                                
106200 IMS-GU-WDL711 SECTION.                                                   
106300     MOVE 'IMS-GU-WDL711   '  TO CURRENT-IMS-SECTION                      
106400                                                                          
106500     MOVE SPACE               TO ALL-SSA                                  
106600     STRING 'WDL701  (IDARTNR  =' W-IDARTNR-X ')'                         
106700          DELIMITED BY SIZE INTO SSA1                                     
106800     STRING 'WDL711  (IDDC     =' W-IDDC-X ')'                            
106900          DELIMITED BY SIZE INTO SSA2                                     
107000     MOVE '  GE' TO GODK-STATUSKODER                                      
107100     CALL CBLTDLI USING GU WDL7-PCB DLI-IO-WDL711 SSA1 SSA2               
107200     MOVE WDL7-STATUS-CODE TO STATUS-WS                                   
107300     PERFORM IMS-STATUSKONTROLL                                           
107400     .                                                                    
107500     EJECT                                                                
107600 IMS-GU-WDD311      SECTION.                                              
107700     MOVE 'IMS-GU-WDD311   '  TO CURRENT-IMS-SECTION                      
107800                                                                          
107900     MOVE SPACE               TO ALL-SSA                                  
108000     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
108100          DELIMITED BY SIZE INTO SSA1                                     
108200     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
108300          DELIMITED BY SIZE INTO SSA2                                     
108400     MOVE '  GE' TO GODK-STATUSKODER                                      
108500     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
108600     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
108700     PERFORM IMS-STATUSKONTROLL                                           
108800     .                                                                    
108900     EJECT                                                                
109000 IMS-GU-WDD902 SECTION.                                                   
109100     MOVE 'IMS-GU-WDD902   '  TO CURRENT-IMS-SECTION                      
109200                                                                          
109300     MOVE SPACE               TO ALL-SSA                                  
109400     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
109500          DELIMITED BY SIZE INTO SSA1                                     
109600     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
109700          DELIMITED BY SIZE INTO SSA2                                     
109800     MOVE '  GE' TO GODK-STATUSKODER                                      
109900     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD902 SSA1 SSA2               
110000     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
110100     PERFORM IMS-STATUSKONTROLL                                           
110200     .                                                                    
110300     EJECT                                                                
110400 IMS-GU-WDB601 SECTION.                                                   
110500     MOVE 'IMS-GU-WDB601   '  TO CURRENT-IMS-SECTION                      
110600                                                                          
110700     MOVE SPACE               TO ALL-SSA                                  
110800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
110900          DELIMITED BY SIZE INTO SSA1                                     
111000     MOVE '  GE' TO GODK-STATUSKODER                                      
111100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
111200     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
111300     PERFORM IMS-STATUSKONTROLL                                           
111400     .                                                                    
111500     EJECT                                                                
111600 IMS-GN-W6D111-HSEQ SECTION.                                              
111700     MOVE 'IMS-GN-W6D111-HS'  TO CURRENT-IMS-SECTION                      
111800                                                                          
111900     MOVE SPACE               TO ALL-SSA                                  
112000     STRING 'W6D111  (W6D1HSEQ =' W-W6D1HSEQ-X ')'                        
112100          DELIMITED BY SIZE INTO SSA1                                     
112200     MOVE '  GEGB' TO GODK-STATUSKODER                                    
112300     CALL CBLTDLI USING GN W6D1-PCB DLI-IO-W6D111 SSA1                    
112400     MOVE W6D1-STATUS-CODE TO STATUS-WS                                   
112500     PERFORM IMS-STATUSKONTROLL                                           
112600     .                                                                    
112700     EJECT                                                                
112800 IMS-GU-WDGX4541 SECTION.                                                 
112900                                                                          
113000     STRING 'WL454101(WDGXKEY  =' W-IDHTYP-X ')'                          
113100          DELIMITED BY SIZE INTO SSA1                                     
113200     MOVE '    ' TO GODK-STATUSKODER                                      
113300     CALL CBLTDLI USING GU  4541-PCB DLI-IO-WDGX4542 SSA1                 
113400     MOVE 4541-STATUS-CODE TO STATUS-WS                                   
113500     PERFORM IMS-STATUSKONTROLL                                           
113600     .                                                                    
113700                                                                          
113800 IMS-GNP-WDGX4542  SECTION.                                               
113900                                                                          
114000     STRING 'WL454111(IDARTNR  =' W-IDARTNR-X                             
114100                    '&IDDC     =' W-IDDC-X                                
114200                    '&KDVORATG <' W-KDVORATG-X ')'                        
114300            DELIMITED BY SIZE INTO SSA1                                   
114400     MOVE '  GE'                TO GODK-STATUSKODER                       
114500     CALL CBLTDLI USING GNP 4541-PCB DLI-IO-WDGX4542 SSA1                 
114600     MOVE 4541-STATUS-CODE      TO STATUS-WS                              
114700     PERFORM IMS-STATUSKONTROLL                                           
114800     .                                                                    
114900                                                                          
115000 IMS-STATUSKONTROLL SECTION.                                              
115100                                                                          
115200     SET STATUS-IX TO 1                                                   
115300     SEARCH GODK-STATUS                                                   
115400       AT END                                                             
115500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
115600         DELIMITED BY SIZE INTO FELTEXT                                   
115700         CALL FELLOG                                                      
115800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
115900         CONTINUE                                                         
116000     END-SEARCH                                                           
116100     .                                                                    
