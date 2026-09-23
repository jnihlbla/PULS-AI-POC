000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W0069100.                                                
000300 AUTHOR.         RICHARD.                                                 
000400 DATE-WRITTEN.   90/11/01.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        ÅTERSTARTSPGM AV LISTOR                                          
000900*        PÅ MVS-SPOOL-PRINTER                                             
001000*        ELLER TILL VALFRI FAX                                            
001100*        STARTAS ÄVEN FRÅN 0601                                           
001200*                                                                         
001300*    INDATA.                                                              
001400*        TRANSAKTION: W0T691                                              
001500*        TRANSAKTION: W0T691X FRÅN 0601                                   
001600*        MID:         W0I69101                                            
001700*                                                                         
001800*    UTDATA.                                                              
001900*        MOD:         W0O69101                                            
002000                                                                          
002100                                                                          
002200 ENVIRONMENT DIVISION.                                                    
002300                                                                          
002400 DATA DIVISION.                                                           
002500     EJECT                                                                
002600 WORKING-STORAGE SECTION.                                                 
002700                                                                          
002800*    -- CHECKED BY WY2000                                                 
002900                                                                          
003000 77  IDPGM                   PIC X(08)   VALUE 'W0069100'.                
003100 77  FELTEXT                 PIC X(80)   VALUE SPACE.                     
003200 77  JA                      PIC X       VALUE 'J'.                       
003300 77  NEJ                     PIC X       VALUE 'N'.                       
003400 77  INDX                    PIC S9(9)   VALUE +1    COMP.                
003500 77  INDX2                   PIC S9(9)   VALUE +1    COMP.                
003600 77  W-PRTTYP                PIC X(5)    VALUE SPACE.                     
003700 77  W-FAXNODE               PIC X(8)    VALUE 'QSERFAX '.                
003800                                                                          
003900 77  NYCKLAR-SW              PIC X       VALUE 'J'.                       
004000     88  NYCKLAR-OK                      VALUE 'J'.                       
004100     88  NYCKLAR-FEL                     VALUE 'N'.                       
004200                                                                          
004300 77  W-IDTRANS               PIC X(4)    VALUE SPACE.                     
004400     88  EGEN-MID                        VALUE '0691'.                    
004500     88  GODK-MID                        VALUE '0601' '0691'.             
004600                                                                          
004700 01  W-VIMSID.                                                            
004800   03  W-IMSID               PIC X(4)    VALUE SPACE.                     
004900   03                        PIC X(4)    VALUE SPACE.                     
005000                                                                          
005100 01  TEST-PRINTER-NAME       PIC X(8).                                    
005200* VT PRINTRAR UTGÅTT. LÄMNAR KVAR VX FÖR EV FRAMTIDA BEHOV.               
005300 88  VX-PRINTER              VALUE 'QSE00000'.                            
005400                                                                          
005500 01  DYNAMISKA-SUBPROGRAM.                                                
005600   03  WMEDKONV              PIC X(8)    VALUE 'WMEDKONV'.                
005700   03  W006PRT               PIC X(8)    VALUE 'W006PRT '.                
005800   03  VIMSID                PIC X(8)    VALUE 'VIMSID  '.                
005900   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
006000   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
006100                                                                          
006200     EJECT                                                                
006300*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
006400*   -COPY WMEDAREA                                                        
006500     SKIP3                                                                
006600 01  MESSAGE-CODES.                                                       
006700   03  INF-PRESS-PF11        PIC X(3)    VALUE '003'.                     
006800   03  INF-PRINT-REQUEST     PIC X(3)    VALUE '118'.                     
006900   03  ERR-WRONG-KEY         PIC X(3)    VALUE '401'.                     
007000   03  ERR-WRONG-BACKUP      PIC X(3)    VALUE '117'.                     
007100     EJECT                                                                
007200 01  FILLER                  PIC X(16)   VALUE 'W006PRT'.                 
007300*    --- PARAMETRAR TILL SUBPROGRAM W006PRT                               
007400*   -COPY W006PRT                                                         
007500     EJECT                                                                
007600 01  FILLER                  PIC X(16)   VALUE 'SPOOL-AREA'.              
007700*01  -COPY WMSGSPOL                                                       
007800                                                                          
007900     EJECT                                                                
008000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008100*                                                                         
008200 01  FILLER                  PIC X(16)   VALUE 'MID-AREA'.                
008300                                                                          
008400*01  -COPY W0I69101                                                       
008500     EJECT                                                                
008600 01  FILLER                  PIC X(16)  VALUE 'MSG/MOD-AREA'.             
008700                                                                          
008800*01  -COPY WMSGAREA                                                       
008900     EJECT                                                                
009000   03  FILLER REDEFINES MSG-AREA.                                         
009100* 05 -COPY W0O69101                                                       
009200     EJECT                                                                
009300 01  FILLER                  PIC X(16)   VALUE 'MFS-AREA'.                
009400*01  -COPY WMFSAREA                                                       
009500     EJECT                                                                
009600******************************************************************        
009700*                                                                         
009800*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009900*                                                                         
010000 01  IMS-WS.                                                              
010100   03  FILLER                PIC X(16)   VALUE 'IMS-WS     '.             
010200                                                                          
010300                                                                          
010400                                                                          
010500*                        **** STATUS-KOD FRÅN IMS                         
010600   03  STATUS-WS             PIC XX.                                      
010700     88  SEGMENT-FINNS                       VALUE '  '.                  
010800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
010900                                                                          
011000   03  GODK-STATUSKODER.                                                  
011100     05  GODK-STATUS OCCURS 3 INDEXED BY STATUS-IX PIC XX.                
011200                                                                          
011300                                                                          
011400                                                                          
011500 01  NYCKLAR-TILL-DLI.                                                    
011600   03  W-WDG801KY-X.                                                      
011700     05  W-IDNODE            PIC X(8)    VALUE SPACE.                     
011800     05  W-TIREGDAT          PIC S9(7)   VALUE ZERO  COMP-3.              
011900     05  W-TIKLOCK-9KOMPL    PIC S9(9)   VALUE ZERO  COMP-3.              
012000                                                                          
012100                                                                          
012200 01  SSA1                    PIC X(64).                                   
012300     EJECT                                                                
012400*                            IMS FUNKTIONSKODER                           
012500*01    -COPY W0003                                                        
012600     EJECT                                                                
012700 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDG801'.           
012800                                                                          
012900 01  DLI-IO-WDG801.                                                       
013000*  03  -COPY WDG801  -PRE WDG8-.                                          
013100                                                                          
013200     EJECT                                                                
013300 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDG811'.           
013400                                                                          
013500 01  DLI-IO-WDG811.                                                       
013600*  03  -COPY WDG811  -PRE WDG8-.                                          
013700                                                                          
013800     EJECT                                                                
013900 LINKAGE SECTION.                                                         
014000*01  -COPY W0009      -PRE MSG-                                           
014100     EJECT                                                                
014200*01  -COPY W0009      -PRE ALT-                                           
014300     EJECT                                                                
014400*01  -COPY W0008      -PRE WDG8-                                          
014500     05  FILLER                  PIC X.                                   
014600     EJECT                                                                
014700 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB WDG8-PCB.                      
014800 MAIN SECTION.                                                            
014900     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB WDG8-PCB.                      
015000                                                                          
015100     PERFORM IMS-GET-MSG                                                  
015200     IF SEGMENT-FINNS                                                     
015300       PERFORM A-INIT                                                     
015400       PERFORM B-KOLLA-NYCKLAR                                            
015500       IF NYCKLAR-OK                                                      
015600         PERFORM IMS-GET-WDG801                                           
015700         IF SEGMENT-FINNS                                                 
015800           IF MFS-UPDATE  OR MFS-UPD-X                                    
015900             IF MID-IDNODE-BACKUP (1:3) = 'FAX'                           
016000                 AND MFS-UPD-X                                            
016100               CONTINUE                                                   
016200             ELSE                                                         
016300               PERFORM D-SKRIV-SIDOR                                      
016400             END-IF                                                       
016500           ELSE                                                           
016600             MOVE INF-PRESS-PF11 TO MED-IDMFSFEL                          
016700             CALL WMEDKONV USING MED-WMEDAREA                             
016800             MOVE MED-MFSFEL    TO MOD-TEMFSFEL                           
016900           END-IF                                                         
017000         ELSE                                                             
017100           MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                             
017200           CALL WMEDKONV USING MED-WMEDAREA                               
017300           MOVE MED-MFSFEL    TO MOD-TEMFSFEL                             
017400         END-IF                                                           
017500       END-IF                                                             
017600       IF MID-IDNODE-BACKUP (1:3) = 'FAX'                                 
017700           OR NOT MFS-UPD-X                                               
017800         COMPUTE MSG-KVLL = LENGTH OF MOD-W0O69101 + 4                    
017900         PERFORM IMS-INSERT-MSG                                           
018000       END-IF                                                             
018100     END-IF                                                               
018200                                                                          
018300     MOVE ZERO TO RETURN-CODE                                             
018400     GOBACK                                                               
018500     .                                                                    
018600     EJECT                                                                
018700 A-INIT SECTION.                                                          
018800                                                                          
018900     IF MSG-DUBBLA-TRANSKODER                                             
019000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W0I69101                 
019100       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
019200       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
019300     ELSE                                                                 
019400       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W0I69101                 
019500       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
019600       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
019700     END-IF                                                               
019800                                                                          
019900     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
020000     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
020100     MOVE MFS-IDTRANS TO W-IDTRANS                                        
020200                                                                          
020300     MOVE LOW-VALUE  TO MSG-AREA                                          
020400     MOVE 'W0O69101' TO MFS-IDMOD                                         
020500     MOVE '0691'     TO MOD-IDTRANS                                       
020600     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
020700                                                                          
020800     MOVE 'GB ' TO MED-IDSKYLT                                            
020900                                                                          
021000     IF GODK-MID                                                          
021100         AND MID-IDNODE NOT = ALL '+'                                     
021200       MOVE MID-IDNODE        TO MOD-IDNODE                               
021300       MOVE MID-TIREGDAT      TO MOD-TIREGDAT                             
021400       MOVE MID-TIKLOCK       TO MOD-TIKLOCK                              
021500       MOVE MID-IDNODE-BACKUP TO MOD-IDNODE-BACKUP                        
021600       MOVE MID-IDTFX         TO MOD-IDTFX                                
021700       MOVE MID-TEFAX (1)     TO MOD-TEFAX (1)                            
021800       MOVE MID-TEFAX (2)     TO MOD-TEFAX (2)                            
021900       MOVE MID-TEFAX (3)     TO MOD-TEFAX (3)                            
022000       MOVE MID-TEFAX (4)     TO MOD-TEFAX (4)                            
022100       MOVE MID-TEFAX (5)     TO MOD-TEFAX (5)                            
022200       IF MOD-IDNODE-BACKUP (1:3) = '+++'                                 
022300         MOVE MFS-RENSA-FAELT   TO MOD-IDNODE-BACKUP                      
022400       END-IF                                                             
022500     ELSE                                                                 
022600       MOVE MFS-RENSA-FAELT   TO MOD-IDNODE                               
022700                                 MOD-TIREGDAT                             
022800                                 MOD-TIKLOCK                              
022900                                 MOD-IDNODE-BACKUP                        
023000                                 MOD-IDTFX                                
023100                                 MOD-TEFAX (1)                            
023200                                 MOD-TEFAX (2)                            
023300                                 MOD-TEFAX (3)                            
023400                                 MOD-TEFAX (4)                            
023500                                 MOD-TEFAX (5)                            
023600     END-IF                                                               
023700     .                                                                    
023800     EJECT                                                                
023900 B-KOLLA-NYCKLAR SECTION.                                                 
024000                                                                          
024100     MOVE MID-IDNODE TO W-IDNODE                                          
024200                                                                          
024300     IF MID-TIREGDAT NUMERIC                                              
024400       MOVE MID-TIREGDAT TO W-TIREGDAT                                    
024500     ELSE                                                                 
024600       MOVE NEJ TO NYCKLAR-SW                                             
024700     END-IF                                                               
024800                                                                          
024900     IF MID-TIKLOCK NUMERIC                                               
025000       MOVE MID-TIKLOCK TO W-TIKLOCK-9KOMPL                               
025100     ELSE                                                                 
025200       MOVE NEJ TO NYCKLAR-SW                                             
025300     END-IF                                                               
025400                                                                          
025500     IF MID-IDNODE-BACKUP (1:3) = '+++' OR 'FAX'                          
025600       MOVE MID-IDNODE TO PRT-IDLTERM                                     
025700     ELSE                                                                 
025800       MOVE MID-IDNODE-BACKUP TO PRT-IDLTERM                              
025900     END-IF                                                               
026000                                                                          
026100     IF NYCKLAR-FEL                                                       
026200       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
026300       CALL WMEDKONV USING MED-WMEDAREA                                   
026400       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
026500     ELSE                                                                 
026600       IF MID-IDNODE-BACKUP (1:3) = 'FAX'                                 
026700           AND MID-IDTFX = SPACE                                          
026800         MOVE '    TYPE IN FAX-NUMBER' TO MOD-TEMFSFEL                    
026900         MOVE NEJ TO NYCKLAR-SW                                           
027000       ELSE                                                               
027100         MOVE 002 TO PRT-KDCALL                                           
027200         CALL W006PRT USING PRT-W006PRT                                   
027300         IF PRT-KDSVAR = 'F'                                              
027400           MOVE ERR-WRONG-BACKUP   TO MED-IDMFSFEL                        
027500           CALL WMEDKONV USING MED-WMEDAREA                               
027600           MOVE MED-MFSFEL         TO MOD-TEMFSFEL                        
027700           MOVE NEJ TO NYCKLAR-SW                                         
027800         END-IF                                                           
027900       END-IF                                                             
028000     END-IF                                                               
028100     .                                                                    
028200     EJECT                                                                
028300 D-SKRIV-SIDOR SECTION.                                                   
028400                                                                          
028500     CALL VIMSID USING W-VIMSID                                           
028600                                                                          
028700     MOVE PRT-BEPRTLST TO W-PRTTYP                                        
028800     MOVE PRT-IDLTERM TO SPOOL-IDNODE                                     
028900                         PRT-IDNODE                                       
029000                                                                          
029100     IF W-PRTTYP = 'LASER' OR 'TERMO' OR 'ZEBRA'                          
029200       MOVE 'IAFP=N1M,PRTO=' TO SPOOL-IAFP                                
029300     END-IF                                                               
029400     IF (W-PRTTYP (1:3) = 'IBM' OR 'FAX' OR 'LAS')                        
029500     AND WDG8-LIST-IDPFDEF NOT = SPACE                                    
029600       MOVE WDG8-LIST-IDPFDEF TO SPOOL-FORMDEF SPOOL-PAGEDEF              
029700       MOVE 'A'  TO SPOOL-CLASS                                           
029800*      -- SET TO CLASS E FOR OLD JES2 PRINTERS                            
029900       IF PRT-IDNODE (1:3) = 'NJO'                                        
030000         MOVE 'E'  TO SPOOL-CLASS                                         
030100       END-IF                                                             
030200                                                                          
030300       MOVE PRT-IDNODE  TO TEST-PRINTER-NAME                              
030400       IF W-IMSID = 'IMG0'                                                
030500         IF VX-PRINTER                                                    
030600           MOVE 'W.????.PSF)' TO SPOOL-USERLIB                            
030700           MOVE 11 TO SPOOL-USERLIB-LENGTH                                
030800         ELSE                                                             
030900           MOVE 'W.QASE.PSF)' TO SPOOL-USERLIB                            
031000           MOVE 11 TO SPOOL-USERLIB-LENGTH                                
031100         END-IF                                                           
031200       ELSE                                                               
031300         IF VX-PRINTER                                                    
031400           MOVE 'W.????.PSF)' TO SPOOL-USERLIB                            
031500           MOVE 11 TO SPOOL-USERLIB-LENGTH                                
031600         ELSE                                                             
031700           MOVE 'W.IGRT.PSF,W.PROD.PSF)' TO SPOOL-USERLIB                 
031800           MOVE 22 TO SPOOL-USERLIB-LENGTH                                
031900         END-IF                                                           
032000       END-IF                                                             
032100                                                                          
032200       MOVE SPOOL-IBM-LASER TO SPOOL-OVR-PARAM                            
032300       COMPUTE SPOOL-IAFP-LL = LENGTH OF SPOOL-NORMAL-OPTIONS             
032400                               + LENGTH OF SPOOL-IBM-LASER-DEL1           
032500                               + SPOOL-USERLIB-LENGTH                     
032600     ELSE                                                                 
032700       COMPUTE SPOOL-IAFP-LL = LENGTH OF SPOOL-NORMAL-OPTIONS             
032800       IF PRT-IDNODE (1:3) = 'R31' OR 'N38'                               
032900         MOVE 'F001' TO SPOOL-FORMS                                       
033000       ELSE                                                               
033100         MOVE 'STD ' TO SPOOL-FORMS                                       
033200       END-IF                                                             
033300     END-IF                                                               
033400     COMPUTE SPOOL-OPT-LL  = SPOOL-IAFP-LL                                
033500                             + LENGTH OF SPOOL-START-OPTIONS              
033600                                                                          
033700     IF MID-IDNODE-BACKUP (1:3) = 'FAX' OR '+++'                          
033800       IF MID-IDTFX NOT = SPACE                                           
033900         MOVE W-FAXNODE TO SPOOL-IDNODE                                   
034000         MOVE 'A'       TO SPOOL-CLASS                                    
034100         MOVE '1'       TO SPOOL-COPIES                                   
034200       END-IF                                                             
034300     END-IF                                                               
034400     PERFORM IMS-CHANGE-SPOOL                                             
034500                                                                          
034600     PERFORM IMS-GNP-WDG8-SIDA                                            
034700     PERFORM UNTIL SEGMENT-SAKNAS                                         
034800       PERFORM DA-MOVE-PAGE                                               
034900       IF W-PRTTYP = 'LASER' OR 'TERMO' OR 'ZEBRA'                        
035000         MOVE +1 TO INDX                                                  
035100         IF WDG8-LIST-IDPRTSPO = 'SPO-A4S '                               
035200           PERFORM 80 TIMES                                               
035300             MOVE SPOOL-A4S-DATA (INDX) TO SPOOL-A4S (INDX)               
035400             ADD +1 TO INDX                                               
035500           END-PERFORM                                                    
035600         ELSE                                                             
035700           PERFORM 70 TIMES                                               
035800             MOVE SPOOL-RAD-DATA (INDX) TO SPOOL-RAD (INDX)               
035900             ADD +1 TO INDX                                               
036000           END-PERFORM                                                    
036100         END-IF                                                           
036200       END-IF                                                             
036300       PERFORM IMS-INSERT-SPOOL                                           
036400       PERFORM IMS-GNP-WDG8-SIDA                                          
036500     END-PERFORM                                                          
036600                                                                          
036700     IF W-FAXNODE = SPOOL-IDNODE                                          
036800       IF GODK-MID AND MID-IDTFX NOT = SPACE                              
036900         MOVE MID-IDTFX TO WDG8-LIST-IDTFX                                
037000         MOVE +1 TO INDX                                                  
037100         PERFORM 5 TIMES                                                  
037200           IF MID-TEFAX (INDX) NOT = SPACE                                
037300             MOVE MID-TEFAX (INDX) TO WDG8-LIST-TEFAX (INDX)              
037400           END-IF                                                         
037500           ADD +1 TO INDX                                                 
037600         END-PERFORM                                                      
037700       END-IF                                                             
037800       MOVE +65  TO SPOOL-FAX-RDW  (1)                                    
037900       MOVE ZERO TO SPOOL-FAX-ZZ   (1)                                    
038000       MOVE '1'  TO SPOOL-FAX-STYR (1)                                    
038100       IF WDG8-LIST-IDTFX = SPACE                                         
038200         MOVE '*AUTOFAX*<TOFAXNUM:62612>'                                 
038300             TO SPOOL-FAX-DATA (1)                                        
038400       ELSE                                                               
038500         MOVE SPACE TO SPOOL-FAX-DATA (1)                                 
038600         STRING '*AUTOFAX*<TOFAXNUM:' WDG8-LIST-IDTFX                     
038700             DELIMITED BY SIZE INTO SPOOL-FAX-DATA (1)                    
038800         INSPECT SPOOL-FAX-DATA (1)                                       
038900             REPLACING FIRST SPACE BY '>'                                 
039000       END-IF                                                             
039100       MOVE +65  TO SPOOL-FAX-RDW  (2)                                    
039200       MOVE ZERO TO SPOOL-FAX-ZZ   (2)                                    
039300       MOVE ' '  TO SPOOL-FAX-STYR (2)                                    
039400       MOVE '<FROMNAME:PULS FAX DISTRIBUTION SYSTEM>'                     
039500             TO SPOOL-FAX-DATA (2)                                        
039600       MOVE +65  TO SPOOL-FAX-RDW  (3)                                    
039700       MOVE ZERO TO SPOOL-FAX-ZZ   (3)                                    
039800       MOVE ' '  TO SPOOL-FAX-STYR (3)                                    
039900       MOVE '<TONAME:FAX RECEIVER>'                                       
040000             TO SPOOL-FAX-DATA (3)                                        
040100       MOVE +1 TO INDX                                                    
040200       PERFORM 5 TIMES                                                    
040300         MOVE +65  TO SPOOL-FAX-RDW  (INDX + 3)                           
040400         MOVE ZERO TO SPOOL-FAX-ZZ   (INDX + 3)                           
040500         MOVE ' '  TO SPOOL-FAX-STYR (INDX + 3)                           
040600         STRING '<NOTE:' WDG8-LIST-TEFAX (INDX) '>   '                    
040700             DELIMITED BY SIZE INTO SPOOL-FAX-DATA (INDX + 3)             
040800         ADD +1 TO INDX                                                   
040900       END-PERFORM                                                        
041000       MOVE +65  TO SPOOL-FAX-RDW  (9)                                    
041100       MOVE ZERO TO SPOOL-FAX-ZZ   (9)                                    
041200       MOVE ' '  TO SPOOL-FAX-STYR (9)                                    
041300       MOVE '<DELETELASTPAGE>'                                            
041400             TO SPOOL-FAX-DATA (9)                                        
041500       PERFORM IMS-INSERT-SPOOL-FAX                                       
041600     END-IF                                                               
041700                                                                          
041800     PERFORM IMS-GET-WDG801                                               
041900     MOVE NEJ TO WDG8-LIST-FLSKRIV                                        
042000     ADD +1   TO WDG8-LIST-KVANTEX-PRINTAD                                
042100     PERFORM IMS-REPL-WDG8                                                
042200                                                                          
042300     IF NOT MFS-UPD-X                                                     
042400       MOVE INF-PRINT-REQUEST TO MED-IDMFSINF                             
042500       CALL WMEDKONV USING MED-WMEDAREA                                   
042600       MOVE MED-MFSINF        TO MOD-TEMFSINF                             
042700     END-IF                                                               
042800     .                                                                    
042900                                                                          
043000     EJECT                                                                
043100 DA-MOVE-PAGE SECTION.                                                    
043200                                                                          
043300     MOVE WDG8-SID-KVLL-BDW TO SPOOL-RAD-BDW                              
043400     MOVE SPACE TO SPOOL-RADER                                            
043500     MOVE +1 TO INDX INDX2                                                
043600     IF WDG8-LIST-IDPRTSPO = 'SPO-A4S'                                    
043700       PERFORM 80 TIMES                                                   
043800         MOVE +85   TO SPOOL-A4S-RDW (INDX)                               
043900         MOVE ZERO  TO SPOOL-A4S-ZZ  (INDX)                               
044000         IF INDX = WDG8-SID-IDPRTRAD (INDX2)                              
044100           MOVE WDG8-SID-TEPRTRAD (INDX2)                                 
044200                TO SPOOL-A4S-DATA (INDX)                                  
044300           ADD +1 TO INDX2                                                
044400         END-IF                                                           
044500         ADD +1 TO INDX                                                   
044600       END-PERFORM                                                        
044700       MOVE '1' TO SPOOL-A4S-STYR (1)                                     
044800     ELSE                                                                 
044900       PERFORM 70 TIMES                                                   
045000         MOVE +137  TO SPOOL-RAD-RDW (INDX)                               
045100         MOVE ZERO  TO SPOOL-RAD-ZZ  (INDX)                               
045200         IF INDX = WDG8-SID-IDPRTRAD (INDX2)                              
045300           MOVE WDG8-SID-TEPRTRAD (INDX2)                                 
045400                TO SPOOL-RAD-DATA (INDX)                                  
045500           ADD +1 TO INDX2                                                
045600         END-IF                                                           
045700         ADD +1 TO INDX                                                   
045800       END-PERFORM                                                        
045900       MOVE '1' TO SPOOL-RAD-STYR (1)                                     
046000     END-IF                                                               
046100     .                                                                    
046200     EJECT                                                                
046300* --- IMS SEKTIONER ---                                                   
046400                                                                          
046500 IMS-GET-MSG SECTION.                                                     
046600                                                                          
046700     MOVE '  QC' TO GODK-STATUSKODER                                      
046800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
046900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
047000     PERFORM IMS-STATUSKONTROLL                                           
047100     .                                                                    
047200                                                                          
047300                                                                          
047400 IMS-INSERT-MSG SECTION.                                                  
047500                                                                          
047600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
047700     MOVE SPACE TO GODK-STATUSKODER                                       
047800     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
047900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
048000     PERFORM IMS-STATUSKONTROLL                                           
048100     .                                                                    
048200                                                                          
048300     EJECT                                                                
048400 IMS-CHANGE-SPOOL SECTION.                                                
048500                                                                          
048600     MOVE SPACE TO GODK-STATUSKODER                                       
048700     CALL CBLTDLI USING CHNG ALT-PCB SPOOL-OPT-DEST                       
048800                             SPOOL-OPTIONS SPOOL-FEEDBACK                 
048900     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
049000     PERFORM IMS-STATUSKONTROLL-SPOOL                                     
049100     .                                                                    
049200                                                                          
049300                                                                          
049400 IMS-INSERT-SPOOL SECTION.                                                
049500                                                                          
049600     MOVE SPACE TO GODK-STATUSKODER                                       
049700     CALL CBLTDLI USING ISRT ALT-PCB SPOOL-RAD-AREA                       
049800     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
049900     PERFORM IMS-STATUSKONTROLL-SPOOL                                     
050000     .                                                                    
050100                                                                          
050200                                                                          
050300 IMS-INSERT-SPOOL-FAX SECTION.                                            
050400                                                                          
050500     MOVE SPACE TO GODK-STATUSKODER                                       
050600     CALL CBLTDLI USING ISRT ALT-PCB SPOOL-FAX-AREA                       
050700     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
050800     PERFORM IMS-STATUSKONTROLL-SPOOL                                     
050900     .                                                                    
051000                                                                          
051100                                                                          
051200     EJECT                                                                
051300 IMS-GET-WDG801   SECTION.                                                
051400                                                                          
051500     STRING 'WDG801  (WDG801KY =' W-WDG801KY-X ')'                        
051600          DELIMITED BY SIZE INTO SSA1                                     
051700     MOVE '  GE' TO GODK-STATUSKODER                                      
051800     CALL CBLTDLI USING GHU WDG8-PCB DLI-IO-WDG801 SSA1                   
051900     MOVE WDG8-STATUS-CODE TO STATUS-WS                                   
052000     PERFORM IMS-STATUSKONTROLL                                           
052100     .                                                                    
052200                                                                          
052300                                                                          
052400 IMS-GNP-WDG8-SIDA SECTION.                                               
052500                                                                          
052600     MOVE SPACE TO DLI-IO-WDG811                                          
052700     MOVE 'WDG811   ' TO SSA1                                             
052800     MOVE '  GE' TO GODK-STATUSKODER                                      
052900     CALL CBLTDLI USING GNP WDG8-PCB DLI-IO-WDG811 SSA1                   
053000     MOVE WDG8-STATUS-CODE TO STATUS-WS                                   
053100     PERFORM IMS-STATUSKONTROLL                                           
053200     .                                                                    
053300                                                                          
053400                                                                          
053500 IMS-REPL-WDG8 SECTION.                                                   
053600                                                                          
053700     MOVE '  ' TO GODK-STATUSKODER                                        
053800     CALL CBLTDLI USING REPL WDG8-PCB DLI-IO-WDG801                       
053900     MOVE WDG8-STATUS-CODE TO STATUS-WS                                   
054000     PERFORM IMS-STATUSKONTROLL                                           
054100     .                                                                    
054200                                                                          
054300     EJECT                                                                
054400 IMS-STATUSKONTROLL-SPOOL SECTION.                                        
054500                                                                          
054600     SET STATUS-IX TO 1                                                   
054700     SEARCH GODK-STATUS                                                   
054800       AT END                                                             
054900         MOVE 'FEL FRÅN IMS SPOOL-INTERFACE SE SPOOL-FEEDBACK'            
055000           TO FELTEXT                                                     
055100         CALL FELLOG                                                      
055200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
055300         CONTINUE                                                         
055400     END-SEARCH                                                           
055500     .                                                                    
055600                                                                          
055700                                                                          
055800 IMS-STATUSKONTROLL SECTION.                                              
055900                                                                          
056000     SET STATUS-IX TO 1                                                   
056100     SEARCH GODK-STATUS                                                   
056200       AT END                                                             
056300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
056400         DELIMITED BY SIZE INTO FELTEXT                                   
056500         CALL FELLOG                                                      
056600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
056700         CONTINUE                                                         
056800     END-SEARCH                                                           
056900     .                                                                    
