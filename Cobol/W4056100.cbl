000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4056100.                                                
000300 AUTHOR.         KJELLSON GÖRAN.                                          
000400 DATE-WRITTEN.   OKTOBER 2014                                             
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        JUSTERING BRUTTOVIKT/VOLYM PÅ DCSE-KOLLI                         
000900*                                                                         
001000*        PROGRAMMET UPPDATERAR WDE6                                       
001100*                                                                         
001200*    INDATA.                                                              
001300*        TRANSAKTION: W4T561, W4T561U                                     
001400*        MID:         W4I56101                                            
001500*                                                                         
001600*    UTDATA.                                                              
001700*        MOD:         W4O56101                                            
001800                                                                          
001900                                                                          
002000 ENVIRONMENT DIVISION.                                                    
002100 DATA DIVISION.                                                           
002200 WORKING-STORAGE SECTION.                                                 
002300                                                                          
002400 77  IDPGM                       PIC X(08)   VALUE 'W4056100'.            
002500 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
002600 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
002700                                                                          
002800*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
002900 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003000                                                                          
003100 77  JA                          PIC X       VALUE 'J'.                   
003200 77  NEJ                         PIC X       VALUE 'N'.                   
003300                                                                          
003400 01  WS-VKORDBTO-NYTT        PIC S9(6)V9(1) COMP-3 VALUE ZERO.            
003410 01  WS-VKORDBTO-CHG         PIC S9(6)V9(1) COMP-3 VALUE ZERO.            
003420                                                                          
003430 01  WS-VLORDBTO-NYTT        PIC S9(4)V9(3) COMP-3 VALUE ZERO.            
003440 01  WS-VLORDBTO-CHG         PIC S9(4)V9(3) COMP-3 VALUE ZERO.            
003450                                                                          
003500*01  -COPY WWDCKONS                                                       
003510*01  -COPY WDECAREA                                                       
003600                                                                          
003700*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
003800                                                                          
003900 77  INDATA-SW                   PIC X       VALUE 'J'.                   
004000     88  INDATA-OK                           VALUE 'J'.                   
004100     88  INDATA-FEL                          VALUE 'N'.                   
004200                                                                          
004300 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
004400     88  NYCKLAR-OK                          VALUE 'J'.                   
004500     88  NYCKLAR-FEL                         VALUE 'N'.                   
004600                                                                          
004700 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
004800     88  EGEN-MID                            VALUE '4561'.                
004900     88  GODK-MID                            VALUE '4561' '4562'          
005000                                                   '4563' '4564'          
005100                                                   '4565' '4566'          
005200                                                   '4567' '4568'          
005300                                                   '4569'.                
005400     88  HELP-MID                            VALUE '0551'.                
005500                                                                          
005600                                                                          
005700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
005800 01  GENERELLA-SUBPROGRAM.                                                
005900     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
006000     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006100     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006400                                                                          
006500*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
006600*01 -COPY WMEDAREA                                                        
006700                                                                          
006800                                                                          
006900 01  MESSAGE-CODES.                                                       
007000     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
007100     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
007200     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
007300     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
007400     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
007500                                                                          
007600                                                                          
007700*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
007800*                                                                         
007900 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008000                                                                          
008100*01 -COPY WMSGINIT                                                        
008200                                                                          
008300                                                                          
008400*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
008500*                                                                         
008600 01  SPAR-AREA.                                                           
008700     03  SPAR-IDTRANS           PIC X(4)    VALUE '4561'.                 
008800                                                                          
008900*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
009000*                                                                         
009100 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009200                                                                          
009300*01  MID -COPY W4I56101                                                   
009400                                                                          
009500 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009600                                                                          
009700*01  -COPY WMSGAREA                                                       
009800                                                                          
009900     03  MOD REDEFINES MSG-AREA.                                          
010000*      05  -COPY W4O56101                                                 
010100                                                                          
010200                                                                          
010300 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010400                                                                          
010500*01  -COPY WMFSAREA                                                       
010600                                                                          
010700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010800*                                                                         
010900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011000                                                                          
011100 01  NYCKLAR-TILL-DLI.                                                    
011200     03  W-IDPRODNR-X.                                                    
011300         05  W-IDPRODNR          PIC S9(7)   VALUE ZERO COMP-3.           
011400     03  W-IDKOLLI-X.                                                     
011500         05  W-IDKOLLI           PIC S9(5)   VALUE ZERO COMP-3.           
011600                                                                          
011700*    --- STATUS KODER FRÅN IMS                                            
011800 01  STATUS-WS                   PIC XX.                                  
011900     88  SEGMENT-FINNS                       VALUE '  '.                  
012000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012200                                                                          
012300 01  GODK-STATUSKODER.                                                    
012400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012500                                                                          
012600 01  ALL-SSA.                                                             
012700     03 SSA1                     PIC X(64).                               
012800     03 SSA2                     PIC X(64).                               
012900                                                                          
013000                                                                          
013100*    --- IMS FUNKTIONSKODER                                               
013200*01  -COPY W0003                                                          
013300                                                                          
013400*    ---  DLI INPUT-OUTPUT AREA                                           
013500                                                                          
013600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE601'.                      
013700 01  DLI-IO-WDE601.                                                       
013800*    03  -COPY WDE601                                                     
013900                                                                          
014000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE611'.                      
014100 01  DLI-IO-WDE611.                                                       
014200*    03  -COPY WDE611                                                     
014300                                                                          
014400                                                                          
014500 LINKAGE SECTION.                                                         
014600*01  -COPY W0009   -PRE MSG-                                              
014700*01  -COPY W0008   -PRE WDP7-                                             
014800     05  FILLER                  PIC X.                                   
014900                                                                          
015000*01  -COPY W0008  -PRE WDE6-                                              
015100     05  FILLER                  PIC X.                                   
015200                                                                          
015300                                                                          
015400                                                                          
015500 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB WDE6-PCB.                     
015600 MAIN SECTION.                                                            
015700     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDE6-PCB.                     
015800                                                                          
015900     PERFORM IMS-GET-MSG                                                  
016000     IF SEGMENT-FINNS                                                     
016100       PERFORM A-INIT                                                     
016200       PERFORM B-KOLLA-NYCKLAR                                            
016300       IF NYCKLAR-OK                                                      
016400         IF MFS-UPDATE                                                    
016500           PERFORM G-KOLLA-INPUT                                          
016600           IF INDATA-OK                                                   
016700             PERFORM H-UPPDATERA                                          
016800           END-IF                                                         
016900         ELSE                                                             
017000           IF MFS-FIRST                                                   
017100             PERFORM C-FOERSTA-SIDA                                       
017200           ELSE                                                           
017300             PERFORM E-SAMMA-SIDA                                         
017400           END-IF                                                         
017500         END-IF                                                           
017600         PERFORM F-LAES-VISA-INFO                                         
017700       END-IF                                                             
017800                                                                          
017900       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O56101 + 4                      
018000       PERFORM IMS-INSERT-MSG                                             
018100     END-IF                                                               
018200                                                                          
018300     MOVE ZERO TO RETURN-CODE                                             
018400     GOBACK                                                               
018500     .                                                                    
018600                                                                          
018700                                                                          
018800 A-INIT SECTION.                                                          
018900     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
019000                                                                          
019100     IF MSG-DUBBLA-TRANSKODER                                             
019200       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I56101                 
019300       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
019400       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
019500     ELSE                                                                 
019600       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I56101                  
019700       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
019800       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
019900     END-IF                                                               
020000                                                                          
020100     MOVE MSG-KDTRTYP      TO MFS-KDTRTYP                                 
020200     MOVE MSG-IDPFK        TO MFS-IDPFK                                   
020300     MOVE MFS-IDTRANS      TO W-IDTRANS                                   
020400                                                                          
020500     MOVE LOW-VALUE        TO MSG-AREA                                    
020600     MOVE 'W4O56101'       TO MFS-IDMOD                                   
020700     MOVE '4561'           TO MOD-IDTRANS                                 
020800     MOVE MFS-RENSA-FAELT  TO MOD-TEMFSFEL MOD-TEMFSINF                   
020900                                                                          
021000     IF EGEN-MID OR HELP-MID                                              
021100       CONTINUE                                                           
021200     ELSE                                                                 
021300       MOVE SPACE TO MFS-KDTRTYP                                          
021400       MOVE '7' TO MFS-IDPFK                                              
021500     END-IF                                                               
021600     .                                                                    
021700                                                                          
021800                                                                          
021900 B-KOLLA-NYCKLAR SECTION.                                                 
022000     MOVE 'B-KOLLA-NYCKLAR ' TO CURRENT-SECTION                           
022100                                                                          
022200     MOVE ALL '+'             TO MSGI-WMSGINIT                            
022300     MOVE '001'               TO MSGI-KDCALL                              
022400     MOVE MSG-LTERM-NAME      TO MSGI-IDLTERM-USER                        
022500     MOVE MSG-SIGNON-USERID   TO MSGI-IDUSER                              
022600     MOVE '4561'              TO MSGI-IDTRANS                             
022700     IF GODK-MID                                                          
022800         MOVE MID-IDPRODNR-IN TO MSGI-IDPRODNR                            
022900         MOVE MID-IDKOLLI-IN  TO MSGI-IDKOLLI                             
023000     END-IF                                                               
023100     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
023200     MOVE MSGI-SPAR-AREA      TO SPAR-AREA                                
023300                                                                          
023400*    - SPRÅK SOM SKA ANVÄNDAS AV WMEDKONV                                 
023500     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
023600                                                                          
023700     MOVE JA TO NYCKLAR-SW                                                
023800                                                                          
023900                                                                          
024000*    -- KONTROLL AV IDPRODNR                                              
024100     MOVE MFS-RENSA-FAELT TO MOD-IDPRODNR-IN                              
024200                                                                          
024300     IF MID-IDPRODNR-IN NOT = ALL '+'                                     
024400       MOVE '7'         TO MFS-IDPFK                                      
024500       MOVE SPACE       TO MFS-KDTRTYP                                    
024600     END-IF                                                               
024700     INSPECT MSGI-IDPRODNR REPLACING LEADING SPACE BY ZERO                
024800     IF MSGI-IDPRODNR NUMERIC                                             
024900       MOVE MSGI-IDPRODNR TO W-IDPRODNR                                   
025000     ELSE                                                                 
025100       MOVE NEJ TO NYCKLAR-SW                                             
025200     END-IF                                                               
025300                                                                          
025400*    -- KONTROLL AV IDKOLLI                                               
025500     MOVE MFS-RENSA-FAELT TO MOD-IDKOLLI-IN                               
025600                                                                          
025700     IF MID-IDKOLLI-IN NOT = ALL '+'                                      
025800       MOVE '7'         TO MFS-IDPFK                                      
025900       MOVE SPACE       TO MFS-KDTRTYP                                    
026000     END-IF                                                               
026100     INSPECT MSGI-IDKOLLI REPLACING LEADING SPACE BY ZERO                 
026200     IF MSGI-IDKOLLI NUMERIC                                              
026300       MOVE MSGI-IDKOLLI TO W-IDKOLLI                                     
026400     ELSE                                                                 
026500       MOVE NEJ TO NYCKLAR-SW                                             
026600     END-IF                                                               
026700                                                                          
026800     IF GODK-MID OR NYCKLAR-OK                                            
026900       MOVE MSGI-IDPRODNR   TO MOD-IDPRODNR-UT                            
026910       INSPECT MOD-IDPRODNR-UT REPLACING LEADING ZERO BY SPACE            
027000       MOVE MSGI-IDKOLLI    TO MOD-IDKOLLI-UT                             
027010       INSPECT MOD-IDKOLLI-UT  REPLACING LEADING ZERO BY SPACE            
027100     ELSE                                                                 
027200       MOVE MFS-RENSA-FAELT TO MOD-IDPRODNR-UT                            
027300       MOVE MFS-RENSA-FAELT TO MOD-IDKOLLI-UT                             
027400     END-IF                                                               
027500                                                                          
027600     IF NYCKLAR-FEL                                                       
027700       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
027800       CALL WMEDKONV USING   MED-WMEDAREA                                 
027900       MOVE MED-MFSFEL    TO MOD-TEMFSFEL                                 
028000       PERFORM MFS-RENSA-FAELT-IN                                         
028100       PERFORM MFS-RENSA-FAELT-UT                                         
028200     END-IF                                                               
028300     .                                                                    
028400                                                                          
028500                                                                          
028600 C-FOERSTA-SIDA SECTION.                                                  
028700     MOVE 'C-FOERSTA-SIDA  ' TO CURRENT-SECTION                           
028800                                                                          
028900     PERFORM MFS-RENSA-FAELT-IN                                           
029000     .                                                                    
029100                                                                          
029200                                                                          
029300 E-SAMMA-SIDA SECTION.                                                    
029400     MOVE 'E-SAMMA-SIDA    ' TO CURRENT-SECTION                           
029500                                                                          
029600     IF EGEN-MID OR HELP-MID                                              
029700       IF MID-VKORDBTO-KOLLI-IN = ALL '+' AND                             
029710          MID-VLORDBTO-KOLLI-IN = ALL '+'                                 
029800         PERFORM MFS-RENSA-FAELT-IN                                       
029900       ELSE                                                               
030000         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
030100         CALL WMEDKONV USING MED-WMEDAREA                                 
030200         MOVE MED-MFSINF     TO MOD-TEMFSINF                              
030300         PERFORM EA-MID-INDATA-TILL-MOD                                   
030400       END-IF                                                             
030500     ELSE                                                                 
030600       PERFORM MFS-RENSA-FAELT-IN                                         
030700     END-IF                                                               
030800     .                                                                    
030900                                                                          
031000                                                                          
031100 EA-MID-INDATA-TILL-MOD SECTION.                                          
031200     MOVE 'EA-MID-TILL-MOG ' TO CURRENT-SECTION                           
031300                                                                          
031400       IF MID-VKORDBTO-KOLLI-IN = ALL '+'                                 
031500*LB      PERFORM MFS-RENSA-FAELT-IN                                       
031510         MOVE MFS-RENSA-FAELT TO MOD-VKORDBTO-KOLLI-IN                    
031600       ELSE                                                               
031700         MOVE MID-VKORDBTO-KOLLI-IN TO MOD-VKORDBTO-KOLLI-IN              
031800         MOVE MFS-ADD-LAES-IN-FAELT TO                                    
031900                  MOD-VKORDBTO-KOLLI-IN-ATTR                              
032000                                                                          
032100       END-IF                                                             
032110                                                                          
032120       IF MID-VLORDBTO-KOLLI-IN = ALL '+'                                 
032130*LB      PERFORM MFS-RENSA-FAELT-IN                                       
032131         MOVE MFS-RENSA-FAELT TO MOD-VLORDBTO-KOLLI-IN                    
032140       ELSE                                                               
032150         MOVE MID-VLORDBTO-KOLLI-IN TO MOD-VLORDBTO-KOLLI-IN              
032160         MOVE MFS-ADD-LAES-IN-FAELT TO                                    
032170                  MOD-VLORDBTO-KOLLI-IN-ATTR                              
032180                                                                          
032190       END-IF                                                             
032200     .                                                                    
032300                                                                          
032400                                                                          
032500 F-LAES-VISA-INFO SECTION.                                                
032600     MOVE 'F-LAES-VISA-INFO' TO CURRENT-SECTION                           
032700                                                                          
032800     PERFORM IMS-GHU-WDE611                                               
032900                                                                          
033000     IF SEGMENT-SAKNAS                                                    
033100        MOVE 'KOLLI SAKNAS' TO MOD-TEMFSFEL                               
033200        PERFORM MFS-RENSA-FAELT-UT                                        
033300     ELSE                                                                 
033310        IF KOLLI-IDDC = WC-DDC-SE                                         
033314           IF KOLLI-KDKOLSTA  = ZERO                                      
033315              MOVE KOLLI-VKORDBTO-KOLLI TO MOD-VKORDBTO-KOLLI-UT          
033316              MOVE KOLLI-VLORDBTO-KOLLI TO MOD-VLORDBTO-KOLLI-UT          
033320           ELSE                                                           
033330              MOVE 'FEL KOLLISTATUS'    TO MOD-TEMFSFEL                   
033331              MOVE NEJ TO INDATA-SW                                       
033340           END-IF                                                         
033360        ELSE                                                              
033390           MOVE 'FEL DC         ' TO MOD-TEMFSFEL                         
033410           PERFORM MFS-RENSA-FAELT-UT                                     
033500        END-IF                                                            
033600     END-IF                                                               
033700     .                                                                    
033800                                                                          
033900                                                                          
034000 G-KOLLA-INPUT SECTION.                                                   
034100     MOVE 'G-KOLLA-INPUT   ' TO CURRENT-SECTION                           
034200                                                                          
034300     MOVE JA  TO INDATA-SW                                                
034400     IF MID-VKORDBTO-KOLLI-IN = ALL '+' AND                               
034410        MID-VLORDBTO-KOLLI-IN = ALL '+'                                   
034500        MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                         
034600        CALL WMEDKONV USING MED-WMEDAREA                                  
034700        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
034800        PERFORM MFS-ROER-EJ-FAELT-IN                                      
034900        PERFORM MFS-ROER-EJ-FAELT-UT                                      
035000        MOVE NEJ TO INDATA-SW                                             
035100     ELSE                                                                 
035200                                                                          
035210        IF MID-VKORDBTO-KOLLI-IN NOT = ALL '+'                            
035220          PERFORM GA-KOLLA-VKORDBTO-KOLLI-IN                              
035221        END-IF                                                            
035222                                                                          
035223        IF MID-VLORDBTO-KOLLI-IN NOT = ALL '+' AND                        
035224          INDATA-OK                                                       
035225          PERFORM GB-KOLLA-VLORDBTO-KOLLI-IN                              
035226        END-IF                                                            
035230     END-IF                                                               
039100     .                                                                    
039200                                                                          
039210 GA-KOLLA-VKORDBTO-KOLLI-IN SECTION.                                      
039220     MOVE 'GA-KOLLA-VKORD  ' TO CURRENT-SECTION                           
039221                                                                          
039222     MOVE MID-VKORDBTO-KOLLI-IN           TO DEC-IDFRIDATA                
039223     MOVE +6                              TO DEC-KVHELTAL                 
039224     MOVE +1                              TO DEC-KVDECIMAL                
039225     CALL WDECEDIT USING DEC-WDECAREA                                     
039226     IF DEC-KDSVAR-FEL                                                    
039227        MOVE NEJ TO INDATA-SW                                             
039228        MOVE MFS-NUM-FAELT-FEL      TO MOD-VKORDBTO-KOLLI-IN-ATTR         
039229     ELSE                                                                 
039230        IF DEC-IDEDITDATA = ZERO                                          
039231           MOVE NEJ TO INDATA-SW                                          
039232          MOVE MFS-NUM-FAELT-FEL    TO MOD-VKORDBTO-KOLLI-IN-ATTR         
039233        ELSE                                                              
039234          MOVE DEC-IDEDITDATA    TO WS-VKORDBTO-NYTT                      
039235        END-IF                                                            
039236     END-IF                                                               
039237                                                                          
039238     IF INDATA-FEL                                                        
039239        MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                         
039240        CALL WMEDKONV USING MED-WMEDAREA                                  
039241        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
039242        PERFORM MFS-ROER-EJ-FAELT-UT                                      
039243        PERFORM MFS-ROER-EJ-FAELT-IN                                      
039244     END-IF                                                               
039246                                                                          
039247     .                                                                    
039248                                                                          
039249 GB-KOLLA-VLORDBTO-KOLLI-IN SECTION.                                      
039250     MOVE 'GB-KOLLA-VLORD  ' TO CURRENT-SECTION                           
039251                                                                          
039252     MOVE MID-VLORDBTO-KOLLI-IN           TO DEC-IDFRIDATA                
039253     MOVE +4                              TO DEC-KVHELTAL                 
039254     MOVE +3                              TO DEC-KVDECIMAL                
039255     CALL WDECEDIT USING DEC-WDECAREA                                     
039256     IF DEC-KDSVAR-FEL                                                    
039257        MOVE NEJ TO INDATA-SW                                             
039258        MOVE MFS-NUM-FAELT-FEL      TO MOD-VLORDBTO-KOLLI-IN-ATTR         
039259     ELSE                                                                 
039260        IF DEC-IDEDITDATA = ZERO                                          
039261           MOVE NEJ TO INDATA-SW                                          
039262          MOVE MFS-NUM-FAELT-FEL    TO MOD-VLORDBTO-KOLLI-IN-ATTR         
039263        ELSE                                                              
039264          MOVE DEC-IDEDITDATA    TO WS-VLORDBTO-NYTT                      
039265        END-IF                                                            
039266     END-IF                                                               
039267                                                                          
039268     IF INDATA-FEL                                                        
039269        MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                         
039270        CALL WMEDKONV USING MED-WMEDAREA                                  
039271        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
039272        PERFORM MFS-ROER-EJ-FAELT-UT                                      
039273        PERFORM MFS-ROER-EJ-FAELT-IN                                      
039274     END-IF                                                               
039275                                                                          
039276     .                                                                    
039280                                                                          
039300                                                                          
039400 H-UPPDATERA SECTION.                                                     
039500     MOVE 'H-UPPDATERA     ' TO CURRENT-SECTION                           
039600                                                                          
039700     PERFORM IMS-GHU-WDE611                                               
039710                                                                          
039800     IF SEGMENT-FINNS                                                     
039810       IF MID-VKORDBTO-KOLLI-IN NOT = ALL '+'                             
039900         COMPUTE WS-VKORDBTO-CHG                                          
039910               = KOLLI-VKORDBTO-KOLLI - WS-VKORDBTO-NYTT                  
040000                                                                          
040100         MOVE WS-VKORDBTO-NYTT TO KOLLI-VKORDBTO-KOLLI                    
040200                                   MOD-VKORDBTO-KOLLI-UT                  
040300       END-IF                                                             
040310                                                                          
040400       IF MID-VLORDBTO-KOLLI-IN NOT = ALL '+'                             
040500         COMPUTE WS-VLORDBTO-CHG                                          
040600               = KOLLI-VLORDBTO-KOLLI - WS-VLORDBTO-NYTT                  
040610                                                                          
040620         MOVE WS-VLORDBTO-NYTT TO KOLLI-VLORDBTO-KOLLI                    
040630                                   MOD-VLORDBTO-KOLLI-UT                  
040640       END-IF                                                             
040650                                                                          
040700       PERFORM IMS-REPL-WDE611                                            
040800       PERFORM IMS-GHU-WDE601                                             
040801                                                                          
040810       IF MID-VKORDBTO-KOLLI-IN NOT = ALL '+'                             
040900         COMPUTE VORD-VKORDBTO = VORD-VKORDBTO - WS-VKORDBTO-CHG          
040910       END-IF                                                             
040920       IF MID-VLORDBTO-KOLLI-IN NOT = ALL '+'                             
040930         COMPUTE VORD-VLORDBTO = VORD-VLORDBTO - WS-VLORDBTO-CHG          
040940       END-IF                                                             
041000       PERFORM IMS-REPL-WDE601                                            
041100                                                                          
041200       MOVE INF-UPDATE-DONE TO MED-IDMFSINF                               
041300       CALL WMEDKONV USING MED-WMEDAREA                                   
041400       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
041500       PERFORM MFS-FORM-ATTR                                              
041600       PERFORM MFS-RENSA-FAELT-IN                                         
041700     END-IF                                                               
041800     .                                                                    
041900                                                                          
042000                                                                          
042100 MFS-RENSA-FAELT-UT SECTION.                                              
042200                                                                          
042300*    --- ALLA UTDATA-FÄLT                                                 
042400     MOVE MFS-RENSA-FAELT TO MOD-VKORDBTO-KOLLI-UT                        
042410     MOVE MFS-RENSA-FAELT TO MOD-VLORDBTO-KOLLI-UT                        
042500     .                                                                    
042600                                                                          
042700 MFS-RENSA-FAELT-IN SECTION.                                              
042800                                                                          
042900*    --- ALLA INDATA-FÄLT                                                 
043000*    MOVE MFS-RENSA-FAELT TO MOD-IDPRODNR-IN                              
043100*                            MOD-IDKOLLI-IN                               
043200     MOVE MFS-RENSA-FAELT TO MOD-VKORDBTO-KOLLI-IN                        
043210                             MOD-VLORDBTO-KOLLI-IN                        
043300     .                                                                    
043400                                                                          
043500 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
043600                                                                          
043700*    --- ALLA UTDATA-FÄLT                                                 
043800*    MOVE MFS-ROER-EJ-FAELT TO MOD-IDPRODNR-UT                            
043900*                              MOD-IDKOLLI-UT                             
044000     MOVE MFS-ROER-EJ-FAELT TO MOD-VKORDBTO-KOLLI-UT                      
044010                               MOD-VLORDBTO-KOLLI-UT                      
044100     .                                                                    
044200                                                                          
044300 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
044400                                                                          
044500*    --- ALLA INDATA-FÄLT                                                 
044600*    MOVE MFS-ROER-EJ-FAELT TO MOD-IDPRODNR-IN                            
044700*                              MOD-IDKOLLI-IN                             
044800     MOVE MFS-ROER-EJ-FAELT TO MOD-VKORDBTO-KOLLI-IN                      
044810                               MOD-VLORDBTO-KOLLI-IN                      
044900     .                                                                    
045000                                                                          
045100 MFS-FORM-ATTR SECTION.                                                   
045200                                                                          
045300*    --- ALLA INDATA-FÄLT                                                 
045400     MOVE MFS-FORMATETS-ATTR TO MOD-VKORDBTO-KOLLI-IN-ATTR                
045410                                MOD-VLORDBTO-KOLLI-IN-ATTR                
045500     .                                                                    
045600                                                                          
045700 MFS-LAES-IN-IGEN SECTION.                                                
045800                                                                          
045900*    --- ALLA INDATA-FÄLT                                                 
046000     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-VKORDBTO-KOLLI-IN-ATTR             
046010                                   MOD-VLORDBTO-KOLLI-IN-ATTR             
046100     .                                                                    
046200                                                                          
046300                                                                          
046400* --- IMS SEKTIONER ---                                                   
046500                                                                          
046600 IMS-GET-MSG SECTION.                                                     
046700                                                                          
046800     MOVE '  QC' TO GODK-STATUSKODER                                      
046900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
047000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
047100     PERFORM IMS-STATUSKONTROLL                                           
047200     .                                                                    
047300                                                                          
047400 IMS-INSERT-MSG SECTION.                                                  
047500                                                                          
047600     IF MSGI-IDLAND-SPR = 'SE'                                            
047700       MOVE '0' TO MFS-KDHUVOMR                                           
047800     END-IF                                                               
047900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
048000     MOVE SPACE TO GODK-STATUSKODER                                       
048100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
048200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
048300     PERFORM IMS-STATUSKONTROLL                                           
048400     .                                                                    
048500                                                                          
048600                                                                          
048700 IMS-GHU-WDE601 SECTION.                                                  
048800     MOVE 'IMS-GHU-WDE601  ' TO CURRENT-IMS-SECTION                       
048900                                                                          
049000     MOVE SPACE            TO ALL-SSA                                     
049100     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
049200       DELIMITED BY SIZE INTO SSA1                                        
049300     MOVE '    '           TO GODK-STATUSKODER                            
049400     CALL CBLTDLI USING GHU WDE6-PCB DLI-IO-WDE601 SSA1                   
049500     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
049600     PERFORM IMS-STATUSKONTROLL                                           
049700     .                                                                    
049800                                                                          
049900                                                                          
050000 IMS-REPL-WDE601 SECTION.                                                 
050100     MOVE 'IMS-REPL-WDE601 ' TO CURRENT-IMS-SECTION                       
050200                                                                          
050300     MOVE SPACE            TO ALL-SSA                                     
050400     MOVE '  '             TO GODK-STATUSKODER                            
050500     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-WDE601                       
050600     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
050700     PERFORM IMS-STATUSKONTROLL                                           
050800     .                                                                    
050900                                                                          
051000                                                                          
051100 IMS-GHU-WDE611 SECTION.                                                  
051200     MOVE 'IMS-GHU-WDE611  ' TO CURRENT-IMS-SECTION                       
051300                                                                          
051400     MOVE SPACE            TO ALL-SSA                                     
051500     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
051600       DELIMITED BY SIZE INTO SSA1                                        
051700     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
051800       DELIMITED BY SIZE INTO SSA2                                        
051900     MOVE '  GE'           TO GODK-STATUSKODER                            
052000     CALL CBLTDLI USING GHU WDE6-PCB DLI-IO-WDE611 SSA1 SSA2              
052100     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
052200     PERFORM IMS-STATUSKONTROLL                                           
052300     .                                                                    
052400                                                                          
052500                                                                          
052600 IMS-REPL-WDE611 SECTION.                                                 
052700     MOVE 'IMS-REPL-WDE611 ' TO CURRENT-IMS-SECTION                       
052800                                                                          
052900     MOVE SPACE            TO ALL-SSA                                     
053000     MOVE '  '             TO GODK-STATUSKODER                            
053100     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-WDE611                       
053200     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
053300     PERFORM IMS-STATUSKONTROLL                                           
053400     .                                                                    
053500                                                                          
053600                                                                          
053700 IMS-STATUSKONTROLL SECTION.                                              
053800                                                                          
053900     SET STATUS-IX TO 1                                                   
054000     SEARCH GODK-STATUS                                                   
054100       AT END                                                             
054200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
054300         DELIMITED BY SIZE INTO FELTEXT                                   
054400         CALL FELLOG                                                      
054500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
054600         CONTINUE                                                         
054700     END-SEARCH                                                           
054800     .                                                                    
