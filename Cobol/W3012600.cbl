000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3012600.                                                
000300 AUTHOR.         BO HAMMARIN.                                             
000400 DATE-WRITTEN.   99/10/08.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PGM UPPDATERAR POÄNGPARAMETRAR FÖR BYTESARTIKLAR                 
000900*                                                                         
001000*        PROGRAMMET UPPDATERAR WDGX3156 (WDR1)                            
001100*                                                                         
001200*    INDATA.                                                              
001300*        TRANSAKTION: W3T126                                              
001400*                     W3T126U                                             
001500*        MID:         W3I12601                                            
001600*                                                                         
001700*    UTDATA.                                                              
001800*        MOD:         W3O12601                                            
001900                                                                          
002000     EJECT                                                                
002100                                                                          
002200 ENVIRONMENT DIVISION.                                                    
002300     EJECT                                                                
002400                                                                          
002500 DATA DIVISION.                                                           
002600 WORKING-STORAGE SECTION.                                                 
002700                                                                          
002800*    -- CHECKED BY WY2000                                                 
002900 77  IDPGM                       PIC X(08)   VALUE 'W3012600'.            
003000                                                                          
003100*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003200 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003300                                                                          
003400 77  JA                          PIC X       VALUE 'J'.                   
003500 77  NEJ                         PIC X       VALUE 'N'.                   
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
004800     88  EGEN-MID                            VALUE '3126'.                
004900     88  GODK-MID                            VALUE '3126'.                
005000     88  HELP-MID                            VALUE '0551'.                
005100 77  SW-FLEXCREP                 PIC X       VALUE 'N'.                   
005200 77  SW-FLEXCBLK                 PIC X       VALUE 'N'.                   
005300 77  SW-REPOINT                  PIC X       VALUE 'N'.                   
005400 77  SW-TIVV-1                   PIC X       VALUE 'N'.                   
005500 77  SW-TIVV-2                   PIC X       VALUE 'N'.                   
005600 77  SW-TIVV-3                   PIC X       VALUE 'N'.                   
005700 77  SW-TIVV-4                   PIC X       VALUE 'N'.                   
005800 77  SW-IDMAIL                   PIC X       VALUE 'N'.                   
005900                                                                          
006000 01    WS-FAELT.                                                          
006100   03  WS-REPOINT                PIC  9(5)V9(4).                          
006200   03  WS-REPOINT-ALFA           PIC  X(10).                              
006300   03  WS-REPOINT-RED            PIC  Z(4)9.9(4).                         
006400   03  WS-TIVV-ALFA              PIC  X(2).                               
006500     EJECT                                                                
006600                                                                          
006700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006800 01  GENERELLA-SUBPROGRAM.                                                
006900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007100     03  W009EMAD                PIC X(8)    VALUE 'W009EMAD'.            
007200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007400     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
007500     EJECT                                                                
007600                                                                          
007700*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007800*01 -COPY WMEDAREA                                                        
007900                                                                          
008000 01  MESSAGE-CODES.                                                       
008100     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
008200     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
008300     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
008400     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
008500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008600     EJECT                                                                
008700                                                                          
008800*01  -COPY WDECAREA                                                       
008900     EJECT                                                                
009000*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
009100*                                                                         
009200 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
009300                                                                          
009400*01 -COPY WMSGINIT                                                        
009500     EJECT                                                                
009600                                                                          
009700*    --- PARAMETRAR TILL SUBPROGRAM W009EMAD (E-ADDRESS VALIDITY)         
009800*                                                                         
009900 01  FILLER                      PIC X(16)  VALUE 'W009EMAD-AREA'.        
010000                                                                          
010100*01 -COPY W009EMAD                                                        
010200     EJECT                                                                
010300                                                                          
010400*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
010500*                                                                         
010600 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
010700*01  MID -COPY W3I12601                                                   
010800     EJECT                                                                
010900                                                                          
011000 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
011100*01  -COPY WMSGAREA                                                       
011200     03  MOD REDEFINES MSG-AREA.                                          
011300*      05  -COPY W3O12601                                                 
011400     EJECT                                                                
011500                                                                          
011600 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
011700*01  -COPY WMFSAREA                                                       
011800     EJECT                                                                
011900                                                                          
012000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
012100*                                                                         
012200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012300 01  NYCKLAR-TILL-DLI.                                                    
012400     03  W-3155-IDHTYP-X.                                                 
012500         05  W-3155-IDHTYP       PIC  X(4)  VALUE '3155'.                 
012600         05  W-3155-LOW-VALUE    PIC  X(26) VALUE LOW-VALUE.              
012700                                                                          
012800     03  W-3156-WDGXKEY-X.                                                
012900         05  W-3156-KDSEGKEY     PIC  X(1)  VALUE '1'.                    
013000                                                                          
013100*    --- STATUS-KOD FRÅN IMS                                              
013200 01  STATUS-WS                   PIC XX.                                  
013300     88  SEGMENT-FINNS                      VALUE '  '.                   
013400     88  SEGMENT-FINNS-REDAN                VALUE 'II'.                   
013500     88  SEGMENT-SAKNAS                     VALUE 'GE'.                   
013600                                                                          
013700 01  GODK-STATUSKODER.                                                    
013800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013900                                                                          
014000 01  SSA1                        PIC X(64).                               
014100 01  SSA2                        PIC X(64).                               
014200     EJECT                                                                
014300                                                                          
014400*    --- IMS FUNKTIONSKODER                                               
014500*01  -COPY W0003                                                          
014600     EJECT                                                                
014700                                                                          
014800*    ---  DLI INPUT-OUTPUT AREA                                           
014900                                                                          
015000 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDGX56'.         
015100 01  DLI-IO-WDGX3156.                                                     
015200*    03  -COPY WDGX3156                                                   
015300     EJECT                                                                
015400                                                                          
015500 LINKAGE SECTION.                                                         
015600*01  -COPY W0009   -PRE MSG-                                              
015700*01  -COPY W0008   -PRE USEA-                                             
015800     05  FILLER                  PIC X.                                   
015900                                                                          
016000*01  -COPY W0008   -PRE 3156-                                             
016100     05  FILLER                  PIC X.                                   
016200     EJECT                                                                
016300                                                                          
016400 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB 3156-PCB.                     
016500 MAIN SECTION.                                                            
016600     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB 3156-PCB.                     
016700                                                                          
016800     PERFORM IMS-GET-MSG                                                  
016900     IF SEGMENT-FINNS                                                     
017000       PERFORM A-INIT                                                     
017100       PERFORM B-KOLLA-NYCKLAR                                            
017200       IF EGEN-MID                                                        
017300         PERFORM G-KOLLA-INPUT                                            
017400       END-IF                                                             
017500       IF INDATA-OK                                                       
017600         IF MFS-UPDATE                                                    
017700           PERFORM H-UPPDATERA                                            
017800         ELSE                                                             
017900           PERFORM E-SAMMA-SIDA                                           
018000         END-IF                                                           
018100       END-IF                                                             
018200       PERFORM F-LAES-VISA-INFO                                           
018300       COMPUTE MSG-KVLL = LENGTH OF MOD-W3O12601 + 4                      
018400       PERFORM IMS-INSERT-MSG                                             
018500     END-IF                                                               
018600                                                                          
018700     MOVE ZERO TO RETURN-CODE                                             
018800     GOBACK                                                               
018900     .                                                                    
019000     EJECT                                                                
019100                                                                          
019200 A-INIT SECTION.                                                          
019300     IF MSG-DUBBLA-TRANSKODER                                             
019400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W3I12601                 
019500       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
019600       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
019700     ELSE                                                                 
019800       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W3I12601                  
019900       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
020000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
020100     END-IF                                                               
020200                                                                          
020300     MOVE MSG-KDTRTYP      TO MFS-KDTRTYP                                 
020400     MOVE MSG-IDPFK        TO MFS-IDPFK                                   
020500     MOVE MFS-IDTRANS      TO W-IDTRANS                                   
020600                                                                          
020700     MOVE LOW-VALUE        TO MSG-AREA                                    
020800     MOVE 'W3O126N1'       TO MFS-IDMOD                                   
020900     MOVE '3126'           TO MOD-IDTRANS                                 
021000     MOVE MFS-RENSA-FAELT  TO MOD-TEMFSFEL MOD-TEMFSINF                   
021100                                                                          
021200     PERFORM MFS-RENSA-FAELT-IN                                           
021300                                                                          
021400     IF EGEN-MID OR HELP-MID                                              
021500       CONTINUE                                                           
021600     ELSE                                                                 
021700       MOVE SPACE          TO MFS-KDTRTYP                                 
021800       MOVE '7'            TO MFS-IDPFK                                   
021900       MOVE NEJ            TO SW-FLEXCREP                                 
022000                              SW-FLEXCBLK                                 
022100                              SW-TIVV-1                                   
022200                              SW-TIVV-2                                   
022300                              SW-TIVV-3                                   
022400                              SW-TIVV-4                                   
022500                              SW-REPOINT                                  
022600                              SW-IDMAIL                                   
022700     END-IF                                                               
022800                                                                          
022900     MOVE 'GB'             TO MED-IDSKYLT                                 
023000     .                                                                    
023100     EJECT                                                                
023200                                                                          
023300 B-KOLLA-NYCKLAR SECTION.                                                 
023400     MOVE ALL '+'           TO MSGI-WMSGINIT                              
023500     MOVE '001'             TO MSGI-KDCALL                                
023600     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
023700     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
023800     MOVE '3126'            TO MSGI-IDTRANS                               
023900                                                                          
024000     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
024100     MOVE '2'               TO MFS-KDMFSFOR                               
024200                                                                          
024300     MOVE MFS-RENSA-FAELT   TO MOD-IDUSER-IN                              
024400     MOVE MSG-SIGNON-USERID TO MOD-IDUSER-UT                              
024500                                                                          
024600     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
024700     .                                                                    
024800     EJECT                                                                
024900                                                                          
025000 E-SAMMA-SIDA SECTION.                                                    
025100                                                                          
025200     IF EGEN-MID OR HELP-MID                                              
025300       IF MID-XPRM-RE0-GRP = ALL '+'                                      
025400         PERFORM MFS-RENSA-FAELT-IN                                       
025500       ELSE                                                               
025600         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
025700         CALL WMEDKONV USING MED-WMEDAREA                                 
025800         MOVE MED-MFSINF     TO MOD-TEMFSINF                              
025900         PERFORM EA-MID-INDATA-TILL-MOD                                   
026000       END-IF                                                             
026100     END-IF                                                               
026200     .                                                                    
026300     EJECT                                                                
026400                                                                          
026500 EA-MID-INDATA-TILL-MOD SECTION.                                          
026600     IF MID-FLEXCREP NOT = ALL '+'                                        
026700       MOVE MID-FLEXCREP           TO MOD-FLEXCREP                        
026800       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-FLEXCREP-ATTR                   
026900       MOVE JA                     TO SW-FLEXCREP                         
027000     END-IF                                                               
027100     IF MID-FLEXCBLK NOT = ALL '+'                                        
027200       MOVE MID-FLEXCBLK           TO MOD-FLEXCBLK                        
027300       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-FLEXCBLK-ATTR                   
027400       MOVE JA                     TO SW-FLEXCBLK                         
027500     END-IF                                                               
027600     IF MID-REPOINT NOT = ALL '+'                                         
027700       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-REPOINT-ATTR                    
027800       MOVE JA                     TO SW-REPOINT                          
027900     END-IF                                                               
028000     IF MID-TIVV-1 NOT = ALL '+'                                          
028100       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-TIVV-1-ATTR                     
028200       MOVE JA                     TO SW-TIVV-1                           
028300     END-IF                                                               
028400     IF MID-TIVV-2 NOT = ALL '+'                                          
028500       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-TIVV-2-ATTR                     
028600       MOVE JA                     TO SW-TIVV-2                           
028700     END-IF                                                               
028800     IF MID-TIVV-3 NOT = ALL '+'                                          
028900       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-TIVV-3-ATTR                     
029000       MOVE JA                     TO SW-TIVV-3                           
029100     END-IF                                                               
029200     IF MID-TIVV-4 NOT = ALL '+'                                          
029300       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-TIVV-4-ATTR                     
029400       MOVE JA                     TO SW-TIVV-4                           
029500     END-IF                                                               
029600     IF MID-IDMAIL NOT = ALL '+'                                          
029700       MOVE MID-IDMAIL             TO MOD-IDMAIL                          
029800       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-IDMAIL-ATTR                     
029900       MOVE JA                     TO SW-IDMAIL                           
030000     END-IF                                                               
030100     .                                                                    
030200     EJECT                                                                
030300                                                                          
030400 F-LAES-VISA-INFO SECTION.                                                
030500     PERFORM IMS-GHU-WDGX3156                                             
030600                                                                          
030700     MOVE MSG-SIGNON-USERID TO MOD-IDUSER-UT                              
030800                                                                          
030900     IF SW-FLEXCREP = NEJ                                                 
031000       MOVE 3156-FLEXCREP           TO MOD-FLEXCREP                       
031100     END-IF                                                               
031200     IF SW-FLEXCBLK = NEJ                                                 
031300       MOVE 3156-FLEXCBLK           TO MOD-FLEXCBLK                       
031400     END-IF                                                               
031500     IF SW-REPOINT = NEJ                                                  
031600       MOVE 3156-REPOINT            TO WS-REPOINT-RED                     
031700       MOVE WS-REPOINT-RED          TO MOD-REPOINT                        
031800     END-IF                                                               
031900     IF SW-TIVV-1 = NEJ                                                   
032000       MOVE 3156-TIVECKNR-BYTDEB(1) TO MOD-TIVV-1                         
032100     END-IF                                                               
032200     IF SW-TIVV-2 = NEJ                                                   
032300       MOVE 3156-TIVECKNR-BYTDEB(2) TO MOD-TIVV-2                         
032400     END-IF                                                               
032500     IF SW-TIVV-3 = NEJ                                                   
032600       MOVE 3156-TIVECKNR-BYTDEB(3) TO MOD-TIVV-3                         
032700     END-IF                                                               
032800     IF SW-TIVV-4 = NEJ                                                   
032900       MOVE 3156-TIVECKNR-BYTDEB(4) TO MOD-TIVV-4                         
033000     END-IF                                                               
033100     IF SW-IDMAIL = NEJ                                                   
033200       MOVE 3156-IDMAIL             TO MOD-IDMAIL                         
033300     END-IF                                                               
033400                                                                          
033500     MOVE '002'                     TO MSGI-KDCALL                        
033600     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
033700     .                                                                    
033800     EJECT                                                                
033900                                                                          
034000 G-KOLLA-INPUT SECTION.                                                   
034100     MOVE JA                         TO INDATA-SW                         
034200                                                                          
034300     IF MID-XPRM-RE0-GRP = ALL '+'                                        
034400       IF MFS-UPDATE                                                      
034500         MOVE ERR-PF11-AND-NO-DATA   TO MED-IDMFSFEL                      
034600         CALL WMEDKONV USING MED-WMEDAREA                                 
034700         MOVE MED-MFSFEL             TO MOD-TEMFSFEL                      
034800         PERFORM MFS-ROER-EJ-FAELT-IN                                     
034900         PERFORM MFS-ROER-EJ-FAELT-UT                                     
035000         MOVE NEJ                    TO INDATA-SW                         
035100       END-IF                                                             
035200     ELSE                                                                 
035300                                                                          
035400       IF MID-IDMAIL = ' '                                                
035500         MOVE MFS-ALFA-FAELT-FEL     TO MOD-IDMAIL-ATTR                   
035600         MOVE NEJ                    TO INDATA-SW                         
035700       ELSE                                                               
035800         MOVE MID-IDMAIL             TO EMAD-IDMAIL                       
035900         CALL W009EMAD USING EMAD-W009EMAD                                
035910         MOVE EMAD-IDMAIL  TO MID-IDMAIL  3156-IDMAIL MOD-IDMAIL          
036000         IF EMAD-KDSVAR > SPACE                                           
036100           MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDMAIL-ATTR                   
036200           MOVE NEJ                  TO INDATA-SW                         
036300         ELSE                                                             
036500           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDMAIL-ATTR                   
036600         END-IF                                                           
036700       END-IF                                                             
036800                                                                          
036900       IF MID-TIVV-4 NOT = ALL '+'                                        
037000         MOVE MID-TIVV-4               TO WS-TIVV-ALFA                    
037100         INSPECT WS-TIVV-ALFA REPLACING ALL ',' BY '0'                    
037200         INSPECT WS-TIVV-ALFA REPLACING ALL '.' BY '0'                    
037300         INSPECT WS-TIVV-ALFA REPLACING ALL '-' BY '0'                    
037400         INSPECT WS-TIVV-ALFA REPLACING ALL '+' BY '0'                    
037500         INSPECT WS-TIVV-ALFA REPLACING ALL '!' BY '0'                    
037600         INSPECT WS-TIVV-ALFA REPLACING ALL '"' BY '0'                    
037700         INSPECT WS-TIVV-ALFA REPLACING ALL '#' BY '0'                    
037800         INSPECT WS-TIVV-ALFA REPLACING ALL '%' BY '0'                    
037900         INSPECT WS-TIVV-ALFA REPLACING ALL '&' BY '0'                    
038000         INSPECT WS-TIVV-ALFA REPLACING ALL '/' BY '0'                    
038100         INSPECT WS-TIVV-ALFA REPLACING ALL '(' BY '0'                    
038200         INSPECT WS-TIVV-ALFA REPLACING ALL ')' BY '0'                    
038300         INSPECT WS-TIVV-ALFA REPLACING ALL '?' BY '0'                    
038400         INSPECT WS-TIVV-ALFA REPLACING ALL ':' BY '0'                    
038500         INSPECT WS-TIVV-ALFA REPLACING ALL '_' BY '0'                    
038600         INSPECT WS-TIVV-ALFA REPLACING ALL '*' BY '0'                    
038700         IF WS-TIVV-ALFA NUMERIC                                          
038800           IF MID-TIVV-4 > 52                                             
038900             MOVE MFS-NUM-FAELT-FEL    TO MOD-TIVV-4-ATTR                 
039000             MOVE NEJ                  TO INDATA-SW                       
039100           ELSE                                                           
039200             MOVE MFS-NUM-FAELT-RAETT  TO MOD-TIVV-4-ATTR                 
039300           END-IF                                                         
039400         ELSE                                                             
039500           MOVE MFS-NUM-FAELT-FEL      TO MOD-TIVV-4-ATTR                 
039600         END-IF                                                           
039700       ELSE                                                               
039800         MOVE MFS-NUM-FAELT-RAETT      TO MOD-TIVV-4-ATTR                 
039900       END-IF                                                             
040000                                                                          
040100       IF MID-TIVV-3 NOT = ALL '+'                                        
040200         MOVE MID-TIVV-3               TO WS-TIVV-ALFA                    
040300         INSPECT WS-TIVV-ALFA REPLACING ALL ',' BY '0'                    
040400         INSPECT WS-TIVV-ALFA REPLACING ALL '.' BY '0'                    
040500         INSPECT WS-TIVV-ALFA REPLACING ALL '-' BY '0'                    
040600         INSPECT WS-TIVV-ALFA REPLACING ALL '+' BY '0'                    
040700         INSPECT WS-TIVV-ALFA REPLACING ALL '!' BY '0'                    
040800         INSPECT WS-TIVV-ALFA REPLACING ALL '"' BY '0'                    
040900         INSPECT WS-TIVV-ALFA REPLACING ALL '#' BY '0'                    
041000         INSPECT WS-TIVV-ALFA REPLACING ALL '%' BY '0'                    
041100         INSPECT WS-TIVV-ALFA REPLACING ALL '&' BY '0'                    
041200         INSPECT WS-TIVV-ALFA REPLACING ALL '/' BY '0'                    
041300         INSPECT WS-TIVV-ALFA REPLACING ALL '(' BY '0'                    
041400         INSPECT WS-TIVV-ALFA REPLACING ALL ')' BY '0'                    
041500         INSPECT WS-TIVV-ALFA REPLACING ALL '?' BY '0'                    
041600         INSPECT WS-TIVV-ALFA REPLACING ALL ':' BY '0'                    
041700         INSPECT WS-TIVV-ALFA REPLACING ALL '_' BY '0'                    
041800         INSPECT WS-TIVV-ALFA REPLACING ALL '*' BY '0'                    
041900         IF WS-TIVV-ALFA NUMERIC                                          
042000           IF MID-TIVV-3 > 52                                             
042100             MOVE MFS-NUM-FAELT-FEL    TO MOD-TIVV-3-ATTR                 
042200             MOVE NEJ                  TO INDATA-SW                       
042300           ELSE                                                           
042400             MOVE MFS-NUM-FAELT-RAETT  TO MOD-TIVV-3-ATTR                 
042500           END-IF                                                         
042600         ELSE                                                             
042700           MOVE MFS-NUM-FAELT-FEL      TO MOD-TIVV-3-ATTR                 
042800           MOVE NEJ                    TO INDATA-SW                       
042900         END-IF                                                           
043000       ELSE                                                               
043100         MOVE MFS-NUM-FAELT-RAETT      TO MOD-TIVV-3-ATTR                 
043200       END-IF                                                             
043300                                                                          
043400       IF MID-TIVV-2 NOT = ALL '+'                                        
043500         MOVE MID-TIVV-2               TO WS-TIVV-ALFA                    
043600         INSPECT WS-TIVV-ALFA REPLACING ALL ',' BY '0'                    
043700         INSPECT WS-TIVV-ALFA REPLACING ALL '.' BY '0'                    
043800         INSPECT WS-TIVV-ALFA REPLACING ALL '-' BY '0'                    
043900         INSPECT WS-TIVV-ALFA REPLACING ALL '+' BY '0'                    
044000         INSPECT WS-TIVV-ALFA REPLACING ALL '!' BY '0'                    
044100         INSPECT WS-TIVV-ALFA REPLACING ALL '"' BY '0'                    
044200         INSPECT WS-TIVV-ALFA REPLACING ALL '#' BY '0'                    
044300         INSPECT WS-TIVV-ALFA REPLACING ALL '%' BY '0'                    
044400         INSPECT WS-TIVV-ALFA REPLACING ALL '&' BY '0'                    
044500         INSPECT WS-TIVV-ALFA REPLACING ALL '/' BY '0'                    
044600         INSPECT WS-TIVV-ALFA REPLACING ALL '(' BY '0'                    
044700         INSPECT WS-TIVV-ALFA REPLACING ALL ')' BY '0'                    
044800         INSPECT WS-TIVV-ALFA REPLACING ALL '?' BY '0'                    
044900         INSPECT WS-TIVV-ALFA REPLACING ALL ':' BY '0'                    
045000         INSPECT WS-TIVV-ALFA REPLACING ALL '_' BY '0'                    
045100         INSPECT WS-TIVV-ALFA REPLACING ALL '*' BY '0'                    
045200         IF WS-TIVV-ALFA NUMERIC                                          
045300           IF MID-TIVV-2 > 52                                             
045400             MOVE MFS-NUM-FAELT-FEL    TO MOD-TIVV-2-ATTR                 
045500             MOVE NEJ                  TO INDATA-SW                       
045600           ELSE                                                           
045700             MOVE MFS-NUM-FAELT-RAETT  TO MOD-TIVV-2-ATTR                 
045800           END-IF                                                         
045900         ELSE                                                             
046000           MOVE MFS-NUM-FAELT-FEL      TO MOD-TIVV-2-ATTR                 
046100           MOVE NEJ                    TO INDATA-SW                       
046200         END-IF                                                           
046300       ELSE                                                               
046400         MOVE MFS-NUM-FAELT-RAETT      TO MOD-TIVV-2-ATTR                 
046500       END-IF                                                             
046600                                                                          
046700       IF MID-TIVV-1 NOT = ALL '+'                                        
046800         MOVE MID-TIVV-1               TO WS-TIVV-ALFA                    
046900         INSPECT WS-TIVV-ALFA REPLACING ALL ',' BY '0'                    
047000         INSPECT WS-TIVV-ALFA REPLACING ALL '.' BY '0'                    
047100         INSPECT WS-TIVV-ALFA REPLACING ALL '-' BY '0'                    
047200         INSPECT WS-TIVV-ALFA REPLACING ALL '+' BY '0'                    
047300         INSPECT WS-TIVV-ALFA REPLACING ALL '!' BY '0'                    
047400         INSPECT WS-TIVV-ALFA REPLACING ALL '"' BY '0'                    
047500         INSPECT WS-TIVV-ALFA REPLACING ALL '#' BY '0'                    
047600         INSPECT WS-TIVV-ALFA REPLACING ALL '%' BY '0'                    
047700         INSPECT WS-TIVV-ALFA REPLACING ALL '&' BY '0'                    
047800         INSPECT WS-TIVV-ALFA REPLACING ALL '/' BY '0'                    
047900         INSPECT WS-TIVV-ALFA REPLACING ALL '(' BY '0'                    
048000         INSPECT WS-TIVV-ALFA REPLACING ALL ')' BY '0'                    
048100         INSPECT WS-TIVV-ALFA REPLACING ALL '?' BY '0'                    
048200         INSPECT WS-TIVV-ALFA REPLACING ALL ':' BY '0'                    
048300         INSPECT WS-TIVV-ALFA REPLACING ALL '_' BY '0'                    
048400         INSPECT WS-TIVV-ALFA REPLACING ALL '*' BY '0'                    
048500         IF WS-TIVV-ALFA NUMERIC                                          
048600           IF MID-TIVV-1 < 01 OR > 52                                     
048700             MOVE MFS-NUM-FAELT-FEL    TO MOD-TIVV-1-ATTR                 
048800             MOVE NEJ                  TO INDATA-SW                       
048900           ELSE                                                           
049000             MOVE MFS-NUM-FAELT-RAETT  TO MOD-TIVV-1-ATTR                 
049100           END-IF                                                         
049200         ELSE                                                             
049300           MOVE MFS-NUM-FAELT-FEL      TO MOD-TIVV-1-ATTR                 
049400           MOVE NEJ                    TO INDATA-SW                       
049500         END-IF                                                           
049600       ELSE                                                               
049700         MOVE MFS-NUM-FAELT-RAETT      TO MOD-TIVV-1-ATTR                 
049800       END-IF                                                             
049900                                                                          
050000       IF MID-REPOINT = ' '                                               
050100         MOVE MFS-NUM-FAELT-FEL      TO MOD-REPOINT-ATTR                  
050200         MOVE NEJ                    TO INDATA-SW                         
050300       ELSE                                                               
050400         IF MID-REPOINT NOT = ALL '+'                                     
050500           MOVE MID-REPOINT            TO WS-REPOINT-ALFA                 
050600           MOVE WS-REPOINT-ALFA        TO DEC-IDFRIDATA                   
050700           MOVE +5                     TO DEC-KVHELTAL                    
050800           MOVE +4                     TO DEC-KVDECIMAL                   
050900                                                                          
051000           CALL WDECEDIT USING DEC-WDECAREA                               
051100                                                                          
051200           IF DEC-KDSVAR-OK                                               
051300             MOVE DEC-IDEDITDATA       TO WS-REPOINT                      
051400             MOVE WS-REPOINT           TO WS-REPOINT-RED                  
051500             MOVE WS-REPOINT-RED       TO MOD-REPOINT                     
051600             MOVE MFS-NUM-FAELT-RAETT  TO MOD-REPOINT-ATTR                
051700           ELSE                                                           
051800             MOVE ZERO                 TO WS-REPOINT                      
051900           END-IF                                                         
052000           IF WS-REPOINT = ZERO                                           
052100             MOVE MFS-NUM-FAELT-FEL    TO MOD-REPOINT-ATTR                
052200             MOVE NEJ                  TO INDATA-SW                       
052300           ELSE                                                           
052400             MOVE MFS-NUM-FAELT-RAETT  TO MOD-REPOINT-ATTR                
052500           END-IF                                                         
052600         ELSE                                                             
052700           MOVE MFS-NUM-FAELT-RAETT    TO MOD-REPOINT-ATTR                
052800         END-IF                                                           
052900       END-IF                                                             
053000                                                                          
053100       IF MID-FLEXCBLK NOT = ALL '+'                                      
053200         IF MID-FLEXCBLK NOT = 'Y' AND 'N'                                
053300           MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLEXCBLK-ATTR                 
053400           MOVE NEJ                  TO INDATA-SW                         
053500         ELSE                                                             
053600           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLEXCBLK-ATTR                 
053700         END-IF                                                           
053800       END-IF                                                             
053900                                                                          
054000       IF MID-FLEXCREP NOT = ALL '+'                                      
054100         IF MID-FLEXCREP NOT = 'Y' AND 'N'                                
054200           MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLEXCREP-ATTR                 
054300           MOVE NEJ                  TO INDATA-SW                         
054400         ELSE                                                             
054500           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLEXCREP-ATTR                 
054600         END-IF                                                           
054700       END-IF                                                             
054800                                                                          
054900       IF INDATA-FEL                                                      
055000         IF MID-IDMAIL   NOT = ALL '+'                                    
055100           MOVE JA                     TO SW-IDMAIL                       
055200         END-IF                                                           
055300         IF MID-TIVV-1   NOT = ALL '+'                                    
055400           MOVE JA                     TO SW-TIVV-1                       
055500         END-IF                                                           
055600         IF MID-TIVV-2   NOT = ALL '+'                                    
055700           MOVE JA                     TO SW-TIVV-2                       
055800         END-IF                                                           
055900         IF MID-TIVV-3   NOT = ALL '+'                                    
056000           MOVE JA                     TO SW-TIVV-3                       
056100         END-IF                                                           
056200         IF MID-TIVV-4   NOT = ALL '+'                                    
056300           MOVE JA                     TO SW-TIVV-4                       
056400         END-IF                                                           
056500         IF MID-REPOINT  NOT = ALL '+'                                    
056600           MOVE JA                     TO SW-REPOINT                      
056700         END-IF                                                           
056800         IF MID-FLEXCBLK NOT = ALL '+'                                    
056900           MOVE JA                     TO SW-FLEXCBLK                     
057000         END-IF                                                           
057100         IF MID-FLEXCREP NOT = ALL '+'                                    
057200           MOVE JA                     TO SW-FLEXCREP                     
057300         END-IF                                                           
057400         MOVE ERR-CORR-HILITE-FLDS   TO MED-IDMFSFEL                      
057500         CALL WMEDKONV USING MED-WMEDAREA                                 
057600         MOVE MED-MFSFEL             TO MOD-TEMFSFEL                      
057700         PERFORM MFS-ROER-EJ-FAELT-UT                                     
057800         PERFORM MFS-ROER-EJ-FAELT-IN                                     
057900*      ELSE                                                               
058000*LÄS IN DATABASSEGMENT FÖR ATT KOLLA INMATNINGSFÄLT                       
058100*        IF SEGMENT-FINNS                                                 
058200*          CONTINUE                                                       
058300*        ELSE                                                             
058400* FLYTTA LÄMPLIGT FELMEDDELANDE                                           
058500*          CALL WMEDKONV USING MED-WMEDAREA                               
058600*          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
058700*          PERFORM MFS-RENSA-FAELT-IN                                     
058800*        END-IF                                                           
058900       END-IF                                                             
059000     END-IF                                                               
059100     .                                                                    
059200     EJECT                                                                
059300                                                                          
059400 H-UPPDATERA SECTION.                                                     
059500     PERFORM IMS-GHU-WDGX3156                                             
059600                                                                          
059700     IF MID-FLEXCREP NOT = ALL '+'                                        
059800       MOVE MID-FLEXCREP          TO 3156-FLEXCREP MOD-FLEXCREP           
059900       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLEXCREP-ATTR                    
060000     ELSE                                                                 
060100       MOVE MFS-ROER-EJ-FAELT     TO MOD-FLEXCREP                         
060200     END-IF                                                               
060300     IF MID-FLEXCBLK NOT = ALL '+'                                        
060400       MOVE MID-FLEXCBLK          TO 3156-FLEXCBLK MOD-FLEXCBLK           
060500       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLEXCBLK-ATTR                    
060600     ELSE                                                                 
060700       MOVE MFS-ROER-EJ-FAELT     TO MOD-FLEXCBLK                         
060800     END-IF                                                               
060900     IF MID-REPOINT NOT = ALL '+'                                         
061000       MOVE WS-REPOINT            TO 3156-REPOINT                         
061100                                     WS-REPOINT-RED                       
061200       MOVE WS-REPOINT-RED        TO MOD-REPOINT                          
061300       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-REPOINT-ATTR                     
061400     ELSE                                                                 
061500       MOVE MFS-ROER-EJ-FAELT     TO MOD-REPOINT                          
061600     END-IF                                                               
061700     IF MID-TIVV-1 NOT = ALL '+'                                          
061800       MOVE MID-TIVV-1            TO 3156-TIVECKNR-BYTDEB(1)              
061900                                     MOD-TIVV-1                           
062000       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TIVV-1-ATTR                      
062100     ELSE                                                                 
062200       MOVE MFS-ROER-EJ-FAELT     TO MOD-TIVV-1                           
062300     END-IF                                                               
062400     IF MID-TIVV-2 NOT = ALL '+'                                          
062500       MOVE MID-TIVV-2            TO 3156-TIVECKNR-BYTDEB(2)              
062600                                     MOD-TIVV-2                           
062700       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TIVV-2-ATTR                      
062800     ELSE                                                                 
062900       MOVE MFS-ROER-EJ-FAELT     TO MOD-TIVV-2                           
063000     END-IF                                                               
063100     IF MID-TIVV-3 NOT = ALL '+'                                          
063200       MOVE MID-TIVV-3            TO 3156-TIVECKNR-BYTDEB(3)              
063300                                     MOD-TIVV-3                           
063400       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TIVV-3-ATTR                      
063500     ELSE                                                                 
063600       MOVE MFS-ROER-EJ-FAELT     TO MOD-TIVV-3                           
063700     END-IF                                                               
063800     IF MID-TIVV-4 NOT = ALL '+'                                          
063900       MOVE MID-TIVV-4            TO 3156-TIVECKNR-BYTDEB(4)              
064000                                     MOD-TIVV-4                           
064100       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TIVV-4-ATTR                      
064200     ELSE                                                                 
064300       MOVE MFS-ROER-EJ-FAELT     TO MOD-TIVV-4                           
064400     END-IF                                                               
064500     IF MID-IDMAIL NOT = ALL '+'                                          
064600       MOVE MID-IDMAIL            TO 3156-IDMAIL MOD-IDMAIL               
064700       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDMAIL-ATTR                      
064800     ELSE                                                                 
064900       MOVE MFS-ROER-EJ-FAELT     TO MOD-IDMAIL                           
065000     END-IF                                                               
065100                                                                          
065200     PERFORM IMS-REPL-WDGX3156                                            
065300                                                                          
065400     MOVE INF-UPDATE-DONE         TO MED-IDMFSINF                         
065500     CALL WMEDKONV USING MED-WMEDAREA                                     
065600     MOVE MED-MFSINF              TO MOD-TEMFSINF                         
065700     PERFORM MFS-FORM-ATTR                                                
065800     PERFORM MFS-RENSA-FAELT-IN                                           
065900     .                                                                    
066000     EJECT                                                                
066100                                                                          
066200 MFS-RENSA-FAELT-IN SECTION.                                              
066300                                                                          
066400*    --- ALLA INDATA-FÄLT                                                 
066500     MOVE MFS-RENSA-FAELT TO MOD-FLEXCREP                                 
066600                             MOD-FLEXCBLK                                 
066700                             MOD-REPOINT                                  
066800                             MOD-TIVV-1                                   
066900                             MOD-TIVV-2                                   
067000                             MOD-TIVV-3                                   
067100                             MOD-TIVV-4                                   
067200                             MOD-IDMAIL                                   
067300     .                                                                    
067400     EJECT                                                                
067500                                                                          
067600 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
067700                                                                          
067800*    --- ALLA UTDATA-FÄLT                                                 
067900     MOVE MFS-ROER-EJ-FAELT TO MOD-IDUSER-UT                              
068000                               MOD-FLEXCREP                               
068100                               MOD-FLEXCBLK                               
068200                               MOD-REPOINT                                
068300                               MOD-TIVV-1                                 
068400                               MOD-TIVV-2                                 
068500                               MOD-TIVV-3                                 
068600                               MOD-TIVV-4                                 
068700                               MOD-IDMAIL                                 
068800     .                                                                    
068900                                                                          
069000 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
069100                                                                          
069200*    --- ALLA INDATA-FÄLT                                                 
069300     MOVE MFS-ROER-EJ-FAELT TO MOD-FLEXCREP                               
069400                               MOD-FLEXCBLK                               
069500                               MOD-REPOINT                                
069600                               MOD-TIVV-1                                 
069700                               MOD-TIVV-2                                 
069800                               MOD-TIVV-3                                 
069900                               MOD-TIVV-4                                 
070000                               MOD-IDMAIL                                 
070100     .                                                                    
070200     EJECT                                                                
070300                                                                          
070400 MFS-FORM-ATTR SECTION.                                                   
070500                                                                          
070600*    --- ALLA INDATA-FÄLT                                                 
070700     MOVE MFS-FORMATETS-ATTR TO MOD-FLEXCREP-ATTR                         
070800                                MOD-FLEXCBLK-ATTR                         
070900                                MOD-REPOINT-ATTR                          
071000                                MOD-TIVV-1-ATTR                           
071100                                MOD-TIVV-2-ATTR                           
071200                                MOD-TIVV-3-ATTR                           
071300                                MOD-TIVV-4-ATTR                           
071400                                MOD-IDMAIL-ATTR                           
071500     .                                                                    
071600     EJECT                                                                
071700                                                                          
071800* --- IMS SEKTIONER ---                                                   
071900                                                                          
072000 IMS-GET-MSG SECTION.                                                     
072100                                                                          
072200     MOVE '  QC'          TO GODK-STATUSKODER                             
072300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
072400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
072500     PERFORM IMS-STATUSKONTROLL                                           
072600     .                                                                    
072700                                                                          
072800 IMS-INSERT-MSG SECTION.                                                  
072900                                                                          
073000     IF MSGI-IDLAND-SPR = 'GB'                                            
073100       MOVE 'N'           TO MFS-KDHUVOMR                                 
073200     END-IF                                                               
073300     MOVE LOW-VALUE       TO MSG-KDZ1 MSG-KDZ2                            
073400     MOVE SPACE           TO GODK-STATUSKODER                             
073500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
073600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
073700     PERFORM IMS-STATUSKONTROLL                                           
073800     .                                                                    
073900     EJECT                                                                
074000                                                                          
074100 IMS-GHU-WDGX3156 SECTION.                                                
074200                                                                          
074300     STRING 'WDR101  (WDGXKEY  =' W-3155-IDHTYP-X ')'                     
074400          DELIMITED BY SIZE INTO SSA1                                     
074500     STRING 'WDGX3156(KDSEGKEY =' W-3156-WDGXKEY-X ')'                    
074600          DELIMITED BY SIZE INTO SSA2                                     
074700     MOVE '  '             TO GODK-STATUSKODER                            
074800     CALL CBLTDLI USING GHU  3156-PCB DLI-IO-WDGX3156 SSA2                
074900     MOVE 3156-STATUS-CODE TO STATUS-WS                                   
075000     PERFORM IMS-STATUSKONTROLL                                           
075100     .                                                                    
075200                                                                          
075300 IMS-REPL-WDGX3156 SECTION.                                               
075400                                                                          
075500     MOVE '  '             TO GODK-STATUSKODER                            
075600     CALL CBLTDLI USING REPL 3156-PCB DLI-IO-WDGX3156                     
075700     MOVE 3156-STATUS-CODE TO STATUS-WS                                   
075800     PERFORM IMS-STATUSKONTROLL                                           
075900     .                                                                    
076000     EJECT                                                                
076100                                                                          
076200 IMS-STATUSKONTROLL SECTION.                                              
076300                                                                          
076400     SET STATUS-IX TO 1                                                   
076500     SEARCH GODK-STATUS                                                   
076600       AT END                                                             
076700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
076800         DELIMITED BY SIZE INTO FELTEXT                                   
076900         CALL FELLOG                                                      
077000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
077100         CONTINUE                                                         
077200     END-SEARCH                                                           
077300     .                                                                    
