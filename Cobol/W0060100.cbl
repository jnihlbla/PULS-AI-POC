000100 ID DIVISION.                                                             
000200 PROGRAM-ID.   W0060100.                                                  
000300 AUTHOR.       CHRISTINA BRUHN.                                           
000400 DATE-WRITTEN. OKTOBER 1990.                                              
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION.                                                            
000800*       ÅTERSTARTS PROGRAM                                                
000900*       AV LISTOR PÅ IMS-SPOOL ELLER FAX                                  
001000*       SKRIVS UT VIA MPP W00691(SPOOL)                                   
001100*                                                                         
001200*                                                                         
001300*    INDATA.                                                              
001400*        TRANSAKTION: W0T601                                              
001500*        MID:         W0I60101                                            
001600*                                                                         
001700*    UTDATA.                                                              
001800*        MOD:         W0O60101                                            
001900*                     W0O69101                                            
002000                                                                          
002100                                                                          
002200 ENVIRONMENT DIVISION.                                                    
002300                                                                          
002400 DATA DIVISION.                                                           
002500                                                                          
002600     EJECT                                                                
002700 WORKING-STORAGE SECTION.                                                 
002800                                                                          
002900*    -- CHECKED BY WY2000                                                 
003000 77  IDPGM                   PIC X(8)    VALUE 'W0060100'.                
003100 77  JA                      PIC X       VALUE 'J'.                       
003200 77  NEJ                     PIC X       VALUE 'N'.                       
003300 77  WS-LISTA-FINNS          PIC X       VALUE 'N'.                       
003400 77  WS-PRINTA-LISTA         PIC X       VALUE 'N'.                       
003500 77  WS-MID-TIKLOCK-RAD      PIC X(9).                                    
003600 77  WS-TIREGDAT             PIC 9(6).                                    
003700 77  WS-FAX                  PIC X(3)    VALUE SPACE.                     
003800 77  IX                      PIC S9(9)   VALUE ZERO COMP SYNC.            
003900 77  INDX                    PIC S9(9)   VALUE ZERO COMP SYNC.            
004000 77  MID-LINE-IX             PIC S9(9)   VALUE ZERO COMP SYNC.            
004100 77  MOD-LINE-IX             PIC S9(9)   VALUE ZERO COMP SYNC.            
004200 77  MAX-IX                  PIC S9(9)   VALUE +12  COMP SYNC.            
004300                                                                          
004400 77  NYCKLAR-SW              PIC X       VALUE 'J'.                       
004500   88  NYCKLAR-OK                        VALUE 'J'.                       
004600   88  NYCKLAR-FEL                       VALUE 'N'.                       
004700 77  PRINTER-SW              PIC X       VALUE 'J'.                       
004800   88  PRINTER-FINNS                     VALUE 'J'.                       
004900 77  W-IDTRANS               PIC X(4)    VALUE  SPACE.                    
005000   88  EGEN-MID                          VALUE '0601'.                    
005100   88  GODK-MID                          VALUE '0601'.                    
005200                                                                          
005300                                                                          
005400 01  DYNAMISKA-SUBPROGRAM.                                                
005500   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
005600   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
005700   03  WMEDKONV              PIC X(8)    VALUE 'WMEDKONV'.                
005800   03  W006PRT               PIC X(8)    VALUE 'W006PRT '.                
005900                                                                          
006000 01  MESSAGES-CODES.                                                      
006100   03  INF-PRESS-PF8         PIC X(3)    VALUE '105'.                     
006200   03  ERR-WRONG-KEY         PIC X(3)    VALUE '401'.                     
006300   03  INF-CHOOSE-LIST       PIC X(3)    VALUE '116'.                     
006400   03  ERR-WRONG-BACKUP      PIC X(3)    VALUE '117'.                     
006500   03  INF-PRINT-REQUEST     PIC X(3)    VALUE '118'.                     
006600   03  ERR-NO-LISTS          PIC X(3)    VALUE '119'.                     
006700     EJECT                                                                
006800*   -COPY W006PRT                                                         
006900     EJECT                                                                
007000*   -COPY WMEDAREA                                                        
007100     EJECT                                                                
007200 01  W-PROG-TO-PROG-SPOOL.                                                
007300   03  FILLER                PIC S9(4)   VALUE +318 COMP SYNC.            
007400   03  FILLER                PIC X(2)    VALUE LOW-VALUE.                 
007500   03  FILLER                PIC X(8)    VALUE 'W00691X '.                
007600   03  FILLER                PIC X(4)    VALUE '0601'.                    
007700   03  FILLER                PIC X(1)    VALUE '1'.                       
007800                                                                          
007900*  03  -COPY W0I69101  -PRE SPOOL-.                                       
008000     EJECT                                                                
008100****************************************************************          
008200*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
008300                                                                          
008400 01  FILLER                  PIC X(8)    VALUE 'MFS-WS  '.                
008500                                                                          
008600 01  KONTROLL-AREA.                                                       
008700*  03  MID -COPY W0I60101                                                 
008800     EJECT                                                                
008900*01  -COPY WMSGAREA                                                       
009000     EJECT                                                                
009100*  03  MOD -COPY W0O60101  -RED MSG-AREA                                  
009200     EJECT                                                                
009300*01  -COPY WMFSAREA                                                       
009400     EJECT                                                                
009500*****************************************************************         
009600*        ARBETSAREOR TILL IMS-SEKTIONERNA                                 
009700*                                                                         
009800 01  IMS-WS.                                                              
009900   03  FILLER                PIC X(8)    VALUE 'IMS-WS  '.                
010000                                                                          
010100                                                                          
010200 01  NYCKLAR-TILL-DLI.                                                    
010300   03  W-WDG801KY-X.                                                      
010400     05  W-IDLTERM           PIC X(8)    VALUE SPACE.                     
010500     05  W-TIREGDAT          PIC S9(7)   VALUE ZERO COMP-3.               
010600     05  W-TIKLOCK-9KOMPL    PIC S9(9)   VALUE ZERO COMP-3.               
010700   03  W-WDG8A1KY-X.                                                      
010800     05  W-IDLTERM-WDG8A     PIC X(8)    VALUE SPACE.                     
010900     05  W-TIREGDAT-WDG8A    PIC S9(7)   VALUE ZERO COMP-3.               
011000     05  W-TIKLOCK-9KOMPL-8A PIC S9(9)   VALUE ZERO COMP-3.               
011100   03  W-WDG8A1KY-LOW-X.                                                  
011200     05  W-IDLTERM-LOW       PIC X(8)    VALUE SPACE.                     
011300     05  W-TIREGDAT-LOW      PIC S9(7)   VALUE ZERO COMP-3.               
011400     05  FILLER              PIC X(5)    VALUE LOW-VALUE.                 
011500   03  W-WDG8A1KY-HIGH-X.                                                 
011600     05  W-IDLTERM-HIGH      PIC X(8)    VALUE SPACE.                     
011700     05  W-TIREGDAT-HIGH     PIC S9(7)   VALUE ZERO COMP-3.               
011800     05  FILLER              PIC X(5)    VALUE HIGH-VALUE.                
011900                                                                          
012000                                                                          
012100*                            *** STATUSKOD FRÅN IMS                       
012200   03  STATUS-WS             PIC XX.                                      
012300       88  SEGMENT-FINNS                 VALUE '  '.                      
012400       88  SEGMENT-SAKNAS                VALUE 'GE'.                      
012500                                                                          
012600                                                                          
012700   03  GODK-STATUSKODER.                                                  
012800     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012900                                                                          
013000                                                                          
013100 01  SSA1                    PIC X(96).                                   
013200 01  SSA2                    PIC X(64).                                   
013300     EJECT                                                                
013400*                            *** IMS FUNKTIONSKODER                       
013500*01  -COPY W0003                                                          
013600     EJECT                                                                
013700*                            *** DLI INPUT-OUTPUT AREA                    
013800 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDG801'.           
013900                                                                          
014000 01  DLI-IO-WDG801.                                                       
014100*  03  -COPY WDG801 -PRE WDG8-.                                           
014200     EJECT                                                                
014300 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDG8AA1'.          
014400                                                                          
014500 01  DLI-IO-WDG8AA1.                                                      
014600*  03  -COPY WDG8A1 -PRE WDG8A-.                                          
014700     EJECT                                                                
014800 LINKAGE SECTION.                                                         
014900*01  -COPY W0009             -PRE MSG-                                    
015000                                                                          
015100*01  -COPY W0009             -PRE ALT-                                    
015200     EJECT                                                                
015300*01  -COPY W0008             -PRE WDG8-                                   
015400      05 FILLER              PIC X.                                       
015500                                                                          
015600*01  -COPY W0008             -PRE WDG8A-                                  
015700      05 FILLER              PIC X.                                       
015800     EJECT                                                                
015900 PROCEDURE DIVISION USING MSG-PCB ALT-PCB                                 
016000                          WDG8-PCB WDG8A-PCB.                             
016100 MAIN SECTION.                                                            
016200     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB                                
016300                           WDG8-PCB WDG8A-PCB.                            
016400                                                                          
016500     PERFORM IMS-GET-MSG                                                  
016600     IF SEGMENT-FINNS                                                     
016700       PERFORM A-INIT-SPARA-INPUT                                         
016800       PERFORM B-KOLLA-NYCKLAR                                            
016900       IF NYCKLAR-OK                                                      
017000         IF MFS-UPDATE                                                    
017100           PERFORM C-KOLLA-BACKUP-PRINTER                                 
017200           IF PRINTER-FINNS                                               
017300              PERFORM D-BEGAER-PRINTNING                                  
017400           END-IF                                                         
017500         ELSE                                                             
017600           IF MFS-FIRST                                                   
017700             PERFORM E-FOERSTA-SIDA                                       
017800           ELSE                                                           
017900             IF MFS-NEXT                                                  
018000               PERFORM F-NAESTA-SIDA                                      
018100             ELSE                                                         
018200               PERFORM G-SAMMA-SIDA                                       
018300             END-IF                                                       
018400           END-IF                                                         
018500           PERFORM H-VISA-LISTRADER                                       
018600         END-IF                                                           
018700       END-IF                                                             
018800       IF WS-FAX NOT = 'FAX'                                              
018900         COMPUTE MSG-KVLL = LENGTH OF MOD-W0O60101 + 4                    
019000         PERFORM IMS-INSERT-MSG                                           
019100       END-IF                                                             
019200     END-IF                                                               
019300                                                                          
019400     MOVE ZERO TO RETURN-CODE                                             
019500     GOBACK                                                               
019600     .                                                                    
019700     EJECT                                                                
019800 A-INIT-SPARA-INPUT SECTION.                                              
019900                                                                          
020000     IF MSG-DUBBLA-TRANSKODER                                             
020100       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W0I60101                 
020200       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
020300       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
020400     ELSE                                                                 
020500       MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W0I60101                   
020600       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
020700       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
020800     END-IF                                                               
020900                                                                          
021000     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
021100     MOVE MSG-IDPFK            TO MFS-IDPFK                               
021200     MOVE MFS-IDTRANS          TO W-IDTRANS                               
021300                                                                          
021400     MOVE LOW-VALUE TO MSG-AREA                                           
021500     MOVE 'W0O60101' TO MFS-IDMOD                                         
021600     MOVE '0601' TO MOD-IDTRANS                                           
021700                                                                          
021800     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                                 
021900                             MOD-TEMFSINF                                 
022000                                                                          
022100     IF NOT EGEN-MID                                                      
022200       MOVE SPACE TO MFS-KDTRTYP                                          
022300       MOVE '7'   TO MFS-IDPFK                                            
022400     END-IF                                                               
022500                                                                          
022600     IF ENGLISH-TEXT                                                      
022700       MOVE 'GB ' TO MED-IDSKYLT                                          
022800     ELSE                                                                 
022900       MOVE 'S  ' TO MED-IDSKYLT                                          
023000     END-IF                                                               
023100     .                                                                    
023200     EJECT                                                                
023300 B-KOLLA-NYCKLAR    SECTION.                                              
023400                                                                          
023500     MOVE MFS-RENSA-FAELT TO MOD-IDLTERM-IN                               
023600                             MOD-IDLIST-IN                                
023700                             MOD-TIREGDAT-IN                              
023800                             MOD-IDLTERM-BACKUP                           
023900                                                                          
024000     IF NOT EGEN-MID                                                      
024100       MOVE MFS-RENSA-FAELT TO MOD-IDLTERM-UT                             
024200                               MOD-IDLIST-UT                              
024300                               MOD-TIREGDAT-UT                            
024400       PERFORM MFS-RENSA-FAELT-UT                                         
024500     ELSE                                                                 
024600       MOVE JA TO NYCKLAR-SW                                              
024700       IF MID-IDLTERM-IN = ALL '+'                                        
024800         MOVE MID-IDLTERM-UT TO MOD-IDLTERM-UT                            
024900       ELSE                                                               
025000         MOVE MID-IDLTERM-IN TO MOD-IDLTERM-UT                            
025100         MOVE '7'             TO MFS-IDPFK                                
025200         MOVE SPACE           TO MFS-KDTRTYP                              
025300       END-IF                                                             
025400                                                                          
025500       IF MOD-IDLTERM-UT = SPACE                                          
025600         MOVE NEJ TO NYCKLAR-SW                                           
025700       ELSE                                                               
025800         MOVE MOD-IDLTERM-UT TO W-IDLTERM                                 
025900                                W-IDLTERM-LOW                             
026000                                W-IDLTERM-HIGH                            
026100                                W-IDLTERM-WDG8A                           
026200       END-IF                                                             
026300                                                                          
026400       IF MID-IDLIST-IN = ALL '+'                                         
026500         MOVE MID-IDLIST-UT TO MOD-IDLIST-UT                              
026600       ELSE                                                               
026700         MOVE MID-IDLIST-IN TO MOD-IDLIST-UT                              
026800         MOVE '7'             TO MFS-IDPFK                                
026900         MOVE SPACE           TO MFS-KDTRTYP                              
027000       END-IF                                                             
027100                                                                          
027200       IF MID-TIREGDAT-IN = ALL '+'                                       
027300         MOVE MID-TIREGDAT-UT TO WS-TIREGDAT                              
027400         INSPECT WS-TIREGDAT REPLACING LEADING SPACE BY ZERO              
027500       ELSE                                                               
027600         MOVE MID-TIREGDAT-IN TO WS-TIREGDAT                              
027700         MOVE '7'             TO MFS-IDPFK                                
027800         MOVE SPACE           TO MFS-KDTRTYP                              
027900       END-IF                                                             
028000                                                                          
028100       IF WS-TIREGDAT NUMERIC AND WS-TIREGDAT > ZERO                      
028200         MOVE WS-TIREGDAT TO W-TIREGDAT                                   
028300                             W-TIREGDAT-LOW                               
028400                             W-TIREGDAT-HIGH                              
028500                             W-TIREGDAT-WDG8A                             
028600       ELSE                                                               
028700         IF WS-TIREGDAT = ZERO                                            
028800           ACCEPT WS-TIREGDAT FROM DATE                                   
028900           MOVE WS-TIREGDAT TO W-TIREGDAT                                 
029000                               W-TIREGDAT-LOW                             
029100                               W-TIREGDAT-HIGH                            
029200                               W-TIREGDAT-WDG8A                           
029300         ELSE                                                             
029400           MOVE NEJ TO NYCKLAR-SW                                         
029500         END-IF                                                           
029600       END-IF                                                             
029700                                                                          
029800       IF GODK-MID OR NYCKLAR-OK                                          
029900         MOVE WS-TIREGDAT TO MOD-TIREGDAT-UT                              
030000       ELSE                                                               
030100         MOVE MFS-RENSA-FAELT TO MOD-IDLTERM-UT                           
030200                                 MOD-IDLIST-UT                            
030300                                 MOD-TIREGDAT-UT                          
030400       END-IF                                                             
030500                                                                          
030600       IF NYCKLAR-FEL                                                     
030700         MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                               
030800         CALL WMEDKONV USING MED-WMEDAREA                                 
030900         MOVE MED-MFSFEL    TO MOD-TEMFSFEL                               
031000         PERFORM MFS-RENSA-FAELT-UT                                       
031100       END-IF                                                             
031200     END-IF                                                               
031300     .                                                                    
031400     EJECT                                                                
031500 C-KOLLA-BACKUP-PRINTER SECTION.                                          
031600                                                                          
031700     MOVE JA TO PRINTER-SW                                                
031800     IF MID-IDLTERM-BACKUP (1:3) = '+++' OR 'FAX'                         
031900       MOVE MID-IDLTERM-BACKUP TO WS-FAX                                  
032000     ELSE                                                                 
032100       MOVE 002                TO PRT-KDCALL                              
032200       MOVE MID-IDLTERM-BACKUP TO PRT-IDLTERM                             
032300       CALL W006PRT USING PRT-W006PRT                                     
032400       MOVE MID-IDLTERM-BACKUP TO MOD-IDLTERM-BACKUP                      
032500       IF PRT-KDSVAR = 'F'                                                
032600         MOVE MFS-ALFA-FAELT-FEL TO MOD-IDLTERM-BACKUP-ATTR               
032700         MOVE NEJ TO PRINTER-SW                                           
032800         MOVE ERR-WRONG-BACKUP TO MED-IDMFSFEL                            
032900         CALL WMEDKONV USING MED-WMEDAREA                                 
033000         MOVE MED-MFSFEL       TO MOD-TEMFSFEL                            
033100         MOVE +12 TO MOD-LINE-IX                                          
033200         PERFORM UNTIL MOD-LINE-IX = +0                                   
033300           IF MID-KDSVAR (MOD-LINE-IX) NOT = ALL '+'                      
033400             MOVE MFS-OEPPNA-ALFA-FAELT TO                                
033500                              MOD-KDSVAR-ATTR (MOD-LINE-IX)               
033600           ELSE                                                           
033700             MOVE MFS-FORMATETS-ATTR TO                                   
033800                                MOD-KDSVAR-ATTR (MOD-LINE-IX)             
033900           END-IF                                                         
034000           MOVE MFS-ROER-EJ-FAELT TO MOD-KDSVAR (MOD-LINE-IX)             
034100                                     MOD-TIKLOCK (MOD-LINE-IX)            
034200                                     MOD-IDLIST (MOD-LINE-IX)             
034300                                     MOD-BEPRTLST(MOD-LINE-IX)            
034400                              MOD-KVANTEX-PRINTAD(MOD-LINE-IX)            
034500                                     MOD-FLSKRIV (MOD-LINE-IX)            
034600           SUBTRACT 1 FROM MOD-LINE-IX                                    
034700         END-PERFORM                                                      
034800       ELSE                                                               
034900         MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLTERM-BACKUP-ATTR             
035000       END-IF                                                             
035100     END-IF                                                               
035200     .                                                                    
035300     EJECT                                                                
035400 D-BEGAER-PRINTNING SECTION.                                              
035500                                                                          
035600     MOVE NEJ TO WS-PRINTA-LISTA                                          
035700     MOVE +12 TO MID-LINE-IX                                              
035800     MOVE MFS-ROER-EJ-FAELT TO MOD-TIKLOCK-9KOMPL-ENTER                   
035900                               MOD-TIKLOCK-9KOMPL-NEXT                    
036000     PERFORM UNTIL MID-LINE-IX = 0                                        
036100       MOVE MID-TIKLOCK-RAD(MID-LINE-IX) TO WS-MID-TIKLOCK-RAD            
036200       IF WS-MID-TIKLOCK-RAD NUMERIC                                      
036300         MOVE MFS-OEPPNA-ALFA-FAELT TO                                    
036400                          MOD-KDSVAR-ATTR    (MID-LINE-IX)                
036500         MOVE MFS-RENSA-FAELT TO                                          
036600                          MOD-KDSVAR         (MID-LINE-IX)                
036700         MOVE MFS-ROER-EJ-FAELT TO                                        
036800                          MOD-TIKLOCK        (MID-LINE-IX)                
036900                          MOD-IDLIST         (MID-LINE-IX)                
037000                          MOD-BEPRTLST       (MID-LINE-IX)                
037100                          MOD-KVANTEX-PRINTAD(MID-LINE-IX)                
037200                          MOD-FLSKRIV        (MID-LINE-IX)                
037300         IF MID-KDSVAR(MID-LINE-IX) = 'X'                                 
037400           COMPUTE W-TIKLOCK-9KOMPL =                                     
037500             (999999999 - MID-TIKLOCK-RAD(MID-LINE-IX))                   
037600           PERFORM IMS-GHU-WDG8-ROT                                       
037700           IF SEGMENT-FINNS                                               
037800             MOVE SPACE              TO SPOOL-MID-W0I69101                
037900             MOVE MOD-IDLTERM-UT     TO SPOOL-MID-IDNODE                  
038000             MOVE MOD-TIREGDAT-UT    TO SPOOL-MID-TIREGDAT                
038100             MOVE W-TIKLOCK-9KOMPL   TO SPOOL-MID-TIKLOCK                 
038200             MOVE MID-IDLTERM-BACKUP TO SPOOL-MID-IDNODE-BACKUP           
038300             MOVE WDG8-LIST-IDTFX    TO SPOOL-MID-IDTFX                   
038400             MOVE +1 TO INDX                                              
038500             PERFORM 5 TIMES                                              
038600               MOVE WDG8-LIST-TEFAX (INDX)                                
038700                 TO SPOOL-MID-TEFAX (INDX)                                
038800               ADD +1 TO INDX                                             
038900             END-PERFORM                                                  
039000             PERFORM IMS-PURGE-ALT-MSG-SPOOL                              
039100             IF WS-FAX = 'FAX'                                            
039200               MOVE +1 TO MID-LINE-IX                                     
039300             ELSE                                                         
039400               MOVE JA TO WDG8-LIST-FLSKRIV                               
039500                          MOD-FLSKRIV(MID-LINE-IX)                        
039600               PERFORM IMS-REPL-WDG8-ROT                                  
039700               MOVE JA TO WS-PRINTA-LISTA                                 
039800             END-IF                                                       
039900           END-IF                                                         
040000         END-IF                                                           
040100       ELSE                                                               
040200         MOVE MFS-FORMATETS-ATTR TO                                       
040300                         MOD-KDSVAR-ATTR    (MID-LINE-IX)                 
040400         MOVE MFS-RENSA-FAELT TO                                          
040500                         MOD-KDSVAR         (MID-LINE-IX)                 
040600                         MOD-TIKLOCK        (MID-LINE-IX)                 
040700                         MOD-IDLIST         (MID-LINE-IX)                 
040800                         MOD-BEPRTLST       (MID-LINE-IX)                 
040900                         MOD-KVANTEX-PRINTAD(MID-LINE-IX)                 
041000                         MOD-FLSKRIV        (MID-LINE-IX)                 
041100       END-IF                                                             
041200       SUBTRACT 1 FROM MID-LINE-IX                                        
041300     END-PERFORM                                                          
041400     IF WS-PRINTA-LISTA = JA                                              
041500       MOVE INF-PRINT-REQUEST      TO MED-IDMFSINF                        
041600       CALL WMEDKONV USING MED-WMEDAREA                                   
041700       MOVE MED-MFSINF             TO MOD-TEMFSINF                        
041800     END-IF                                                               
041900     .                                                                    
042000     EJECT                                                                
042100 E-FOERSTA-SIDA     SECTION.                                              
042200                                                                          
042300     PERFORM IMS-GN-WDG8A-RADER                                           
042400     .                                                                    
042500     EJECT                                                                
042600 F-NAESTA-SIDA     SECTION.                                               
042700                                                                          
042800     IF MID-TIKLOCK-9KOMPL-NEXT > ZERO                                    
042900       MOVE MID-TIKLOCK-9KOMPL-NEXT TO W-TIKLOCK-9KOMPL-8A                
043000       PERFORM IMS-GU-WDG8A-RAD                                           
043100     ELSE                                                                 
043200       PERFORM IMS-GN-WDG8A-RADER                                         
043300     END-IF                                                               
043400     .                                                                    
043500     EJECT                                                                
043600 G-SAMMA-SIDA     SECTION.                                                
043700                                                                          
043800     IF MID-TIKLOCK-9KOMPL-ENTER > ZERO                                   
043900       MOVE MID-TIKLOCK-9KOMPL-ENTER TO W-TIKLOCK-9KOMPL-8A               
044000       PERFORM IMS-GU-WDG8A-RAD                                           
044100     ELSE                                                                 
044200       PERFORM IMS-GN-WDG8A-RADER                                         
044300     END-IF                                                               
044400     .                                                                    
044500     EJECT                                                                
044600 H-VISA-LISTRADER       SECTION.                                          
044700                                                                          
044800     MOVE +1 TO IX                                                        
044900     IF SEGMENT-FINNS                                                     
045000       MOVE WDG8A-SEQA-TIKLOCK-9KOMPL TO                                  
045100                  MOD-TIKLOCK-9KOMPL-ENTER                                
045200     ELSE                                                                 
045300       MOVE ZERO TO MOD-TIKLOCK-9KOMPL-ENTER                              
045400     END-IF                                                               
045500                                                                          
045600     PERFORM UNTIL IX > MAX-IX                                            
045700       IF SEGMENT-FINNS                                                   
045800         IF MOD-IDLIST-UT = SPACE OR                                      
045900            MOD-IDLIST-UT = WDG8A-SEQA-IDLIST                             
046000           MOVE MFS-OEPPNA-ALFA-FAELT TO                                  
046100                MOD-KDSVAR-ATTR(IX)                                       
046200           COMPUTE MOD-TIKLOCK(IX) =                                      
046300           (999999999 - WDG8A-SEQA-TIKLOCK-9KOMPL)                        
046400           MOVE WDG8A-SEQA-IDLIST  TO MOD-IDLIST (IX)                     
046500                                                                          
046600           MOVE 001                TO PRT-KDCALL                          
046700           MOVE WDG8A-SEQA-IDPRTLST TO PRT-IDPRTLST                       
046800           CALL W006PRT USING PRT-W006PRT                                 
046900           MOVE PRT-BEPRTLST TO MOD-BEPRTLST(IX)                          
047000                                                                          
047100           MOVE WDG8A-SEQA-KVANTEX-PRINTAD TO                             
047200                      MOD-KVANTEX-PRINTAD(IX)                             
047300           MOVE WDG8A-SEQA-FLSKRIV   TO MOD-FLSKRIV(IX)                   
047400           IF WDG8A-SEQA-FLSKRIV = JA                                     
047500             MOVE MFS-FORMATETS-ATTR TO MOD-KDSVAR-ATTR(IX)               
047600           END-IF                                                         
047700           MOVE MFS-RENSA-FAELT TO MOD-KDSVAR(IX)                         
047800           MOVE JA              TO WS-LISTA-FINNS                         
047900           ADD +1 TO IX                                                   
048000         END-IF                                                           
048100         PERFORM IMS-GN-WDG8A-RADER                                       
048200       ELSE                                                               
048300         MOVE MFS-FORMATETS-ATTR TO MOD-KDSVAR-ATTR(IX)                   
048400         MOVE MFS-RENSA-FAELT    TO MOD-KDSVAR(IX)                        
048500                                    MOD-TIKLOCK(IX)                       
048600                                    MOD-IDLIST (IX)                       
048700                                    MOD-BEPRTLST(IX)                      
048800                                    MOD-KVANTEX-PRINTAD(IX)               
048900                                    MOD-FLSKRIV(IX)                       
049000         ADD +1 TO IX                                                     
049100       END-IF                                                             
049200     END-PERFORM                                                          
049300     IF WS-LISTA-FINNS = JA                                               
049400       MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLTERM-BACKUP-ATTR               
049500       MOVE INF-CHOOSE-LIST      TO MED-IDMFSINF                          
049600       CALL WMEDKONV USING MED-WMEDAREA                                   
049700       MOVE MED-MFSINF           TO MOD-TEMFSINF                          
049800     ELSE                                                                 
049900       MOVE MFS-FORMATETS-ATTR TO MOD-IDLTERM-BACKUP-ATTR                 
050000       MOVE ERR-NO-LISTS       TO MED-IDMFSFEL                            
050100       CALL WMEDKONV USING MED-WMEDAREA                                   
050200       MOVE MED-MFSFEL         TO MOD-TEMFSFEL                            
050300     END-IF                                                               
050400     IF SEGMENT-FINNS                                                     
050500       MOVE WDG8A-SEQA-TIKLOCK-9KOMPL TO MOD-TIKLOCK-9KOMPL-NEXT          
050600       MOVE INF-PRESS-PF8            TO MED-IDMFSINF                      
050700       CALL WMEDKONV USING MED-WMEDAREA                                   
050800       MOVE MED-MFSINF               TO MOD-TEMFSINF                      
050900     ELSE                                                                 
051000       MOVE ZERO TO MOD-TIKLOCK-9KOMPL-NEXT                               
051100     END-IF                                                               
051200     .                                                                    
051300     EJECT                                                                
051400 MFS-RENSA-FAELT-UT SECTION.                                              
051500                                                                          
051600     MOVE +1 TO IX                                                        
051700     MOVE MFS-RENSA-FAELT TO MOD-IDLTERM-BACKUP                           
051800                             MOD-TIKLOCK-9KOMPL-ENTER                     
051900                             MOD-TIKLOCK-9KOMPL-NEXT                      
052000                                                                          
052100     PERFORM UNTIL IX > MAX-IX                                            
052200       MOVE MFS-RENSA-FAELT TO MOD-KDSVAR(IX)                             
052300                               MOD-TIKLOCK(IX)                            
052400                               MOD-IDLIST (IX)                            
052500                               MOD-KVANTEX-PRINTAD(IX)                    
052600                               MOD-FLSKRIV(IX)                            
052700       ADD +1 TO IX                                                       
052800     END-PERFORM                                                          
052900     .                                                                    
053000     EJECT                                                                
053100* IMS SECTIONER                                                           
053200                                                                          
053300 IMS-GET-MSG SECTION.                                                     
053400     MOVE '  QC' TO GODK-STATUSKODER                                      
053500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
053600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
053700     PERFORM IMS-STATUS-KONTROLL                                          
053800     .                                                                    
053900                                                                          
054000                                                                          
054100 IMS-INSERT-MSG SECTION.                                                  
054200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
054300     IF ENGLISH-TEXT                                                      
054400       MOVE 'N' TO MFS-KDHUVOMR                                           
054500     END-IF                                                               
054600     MOVE SPACE TO GODK-STATUSKODER                                       
054700     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
054800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
054900     PERFORM IMS-STATUS-KONTROLL                                          
055000     .                                                                    
055100                                                                          
055200                                                                          
055300 IMS-PURGE-ALT-MSG-SPOOL SECTION.                                         
055400     MOVE SPACE TO GODK-STATUSKODER                                       
055500     CALL CBLTDLI USING PURG ALT-PCB W-PROG-TO-PROG-SPOOL                 
055600     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
055700     PERFORM IMS-STATUS-KONTROLL                                          
055800     .                                                                    
055900     EJECT                                                                
056000 IMS-GU-WDG8A-RAD      SECTION.                                           
056100     STRING 'WLLISC01(WDG8A1KY>=' W-WDG8A1KY-X ')'                        
056200             DELIMITED BY SIZE INTO SSA1                                  
056300     MOVE '  GBGE' TO GODK-STATUSKODER                                    
056400     CALL CBLTDLI USING GU WDG8A-PCB DLI-IO-WDG8AA1 SSA1                  
056500     MOVE WDG8A-STATUS-CODE TO STATUS-WS                                  
056600     PERFORM IMS-STATUS-KONTROLL                                          
056700     .                                                                    
056800                                                                          
056900                                                                          
057000 IMS-GN-WDG8A-RADER    SECTION.                                           
057100     STRING 'WLLISC01(WDG8A1KY>=' W-WDG8A1KY-LOW-X                        
057200                    '&WDG8A1KY<=' W-WDG8A1KY-HIGH-X ')'                   
057300             DELIMITED BY SIZE INTO SSA1                                  
057400     MOVE '  GBGE' TO GODK-STATUSKODER                                    
057500     CALL CBLTDLI USING GN WDG8A-PCB DLI-IO-WDG8AA1 SSA1                  
057600     MOVE WDG8A-STATUS-CODE TO STATUS-WS                                  
057700     PERFORM IMS-STATUS-KONTROLL                                          
057800     .                                                                    
057900                                                                          
058000                                                                          
058100 IMS-GHU-WDG8-ROT     SECTION.                                            
058200     STRING 'WLLISB01(WDG801KY =' W-WDG801KY-X ')'                        
058300             DELIMITED BY SIZE INTO SSA1                                  
058400     MOVE '  GE' TO GODK-STATUSKODER                                      
058500     CALL CBLTDLI USING GHU WDG8-PCB DLI-IO-WDG801 SSA1                   
058600     MOVE WDG8-STATUS-CODE TO STATUS-WS                                   
058700     PERFORM IMS-STATUS-KONTROLL                                          
058800     .                                                                    
058900                                                                          
059000                                                                          
059100 IMS-REPL-WDG8-ROT SECTION.                                               
059200     MOVE '  ' TO GODK-STATUSKODER                                        
059300     CALL CBLTDLI USING REPL WDG8-PCB DLI-IO-WDG801                       
059400     MOVE WDG8-STATUS-CODE TO STATUS-WS                                   
059500     PERFORM IMS-STATUS-KONTROLL                                          
059600     .                                                                    
059700     EJECT                                                                
059800 IMS-STATUS-KONTROLL SECTION.                                             
059900     SET STATUS-IX TO 1                                                   
060000     SEARCH GODK-STATUS                                                   
060100       AT END                                                             
060200         CALL FELLOG                                                      
060300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
060400         CONTINUE                                                         
060500     END-SEARCH                                                           
060600     .                                                                    
