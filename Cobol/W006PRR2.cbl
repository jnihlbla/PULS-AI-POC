000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W006PRR2.                                                
000400 AUTHOR.         RICHARD.                                                 
000500 DATE-WRITTEN.   AUG.  93.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION.                                                            
000900*        GENERELLT LISTNINGSPROGRAM                                       
001000*        VIA SPOOL-API MED ÅTERSTART                                      
001100*        TILL PRINTER ELLER FAX                                           
001200*                                                                         
001300*    INDATA.                                                              
001400*        CALL-PARAMETRAR FRÅN KALLANDE PROGRAM.                           
001500*                                                                         
001600*    UTDATA.                                                              
001700*        SIDOR TILL SPOOL-API                                             
001800                                                                          
001900 ENVIRONMENT DIVISION.                                                    
002000                                                                          
002100 DATA DIVISION.                                                           
002200     EJECT                                                                
002300 WORKING-STORAGE SECTION.                                                 
002400                                                                          
002500*    -- CHECKED BY WY2000                                                 
002600 77  IDPGM                   PIC X(8)    VALUE 'W006PRR2'.                
002700 77  FELTEXT                 PIC X(80)   VALUE SPACE.                     
002800 77  W-MAX-IX                PIC S9(9)   VALUE +1    COMP.                
002900 77  INDX                    PIC S9(9)   VALUE +1    COMP.                
003000 77  INDX2                   PIC S9(9)   VALUE +1    COMP.                
003100 77  W-IX                    PIC S9(9)   VALUE +1    COMP.                
003200 77  W-RADL                  PIC S9(4)   VALUE +137  COMP.                
003300 77  W-NYSIDA                PIC S9(3)   VALUE +0    COMP-3.              
003400 77  W-SKIP                  PIC S9(2)   VALUE +1    COMP-3.              
003500 77  JA                      PIC X       VALUE 'J'.                       
003600 77  NEJ                     PIC X       VALUE 'N'.                       
003700 77  W-SKRIV-SW              PIC X       VALUE 'N'.                       
003800 77  W-FAX-SW                PIC X       VALUE 'N'.                       
003900 77  W-CHANGE-SW             PIC X       VALUE 'J'.                       
004000 77  W-PRT-NYLISTA           PIC X       VALUE 'J'.                       
004100 77  W-IDPRTLST              PIC X(8)    VALUE HIGH-VALUE.                
004200 77  W-IDPRTLST-SPAR         PIC X(8)    VALUE HIGH-VALUE.                
004300 77  W-DATUM                 PIC S9(7)   VALUE ZERO  COMP-3.              
004400 77  W-TIME                  PIC S9(9)   VALUE ZERO  COMP-3.              
004500 77  W-NIOR                  PIC S9(9)   VALUE +999999999 COMP-3.         
004600 77  W-IDSID                 PIC S9(3)   VALUE ZERO  COMP-3.              
004700 77  W-PRTTYP                PIC X(5)    VALUE SPACE.                     
004800                                                                          
004900 01  W-IDENT.                                                             
005000   03  W-IDLIST              PIC X(10)   VALUE HIGH-VALUE.                
005100   03  FILLER                PIC X(10)   VALUE SPACE.                     
005200   03  W-IDPGM               PIC X(8)    VALUE SPACE.                     
005300                                                                          
005400 01  W-VIMSID.                                                            
005500   03  W-IMSID               PIC X(4)    VALUE SPACE.                     
005600   03                        PIC X(4)    VALUE SPACE.                     
005700                                                                          
005800 01  TEST-PRINTER-NAME       PIC X(8).                                    
005810* V6+VT PRINTRAR UTGÅTT. LÄMNAR KVAR VX FÖR EV FRAMTIDA BEHOV.            
005820 88  VX-PRINTER              VALUE 'QSE00000'.                            
005830                                                                          
006400 01  DYNAMISKA-SUBPROGRAM.                                                
006500   03  W006PRT               PIC X(8)    VALUE 'W006PRT '.                
006600   03  VIMSID                PIC X(8)    VALUE 'VIMSID  '.                
006700   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
006800   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
006900                                                                          
007000     EJECT                                                                
007100 01  FILLER                  PIC X(16)   VALUE 'W006PRT '.                
007200*01  -COPY W006PRT.                                                       
007300                                                                          
007400     EJECT                                                                
007500 01  FILLER                  PIC X(16)   VALUE 'W006PRTY'.                
007600*01  -COPY W006PRTY.                                                      
007700                                                                          
007800     EJECT                                                                
007900 01  FILLER                  PIC X(16)   VALUE 'W006PRAR'.                
008000*01  -COPY W006PRAR.                                                      
008100                                                                          
008200     EJECT                                                                
008300 01  FILLER                  PIC X(16)   VALUE 'SPOOL-AREA'.              
008400*01  -COPY WMSGSPOL                                                       
008500                                                                          
008600     EJECT                                                                
008700******************************************************************        
008800*                                                                         
008900*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009000*                                                                         
009100 01  IMS-WS.                                                              
009200   03  FILLER                PIC X(16)   VALUE 'IMS-WS     '.             
009300     SKIP3                                                                
009400*                        **** STATUS-KOD FRÅN IMS                         
009500   03  STATUS-WS             PIC XX.                                      
009600     88  SEGMENT-FINNS                   VALUE '  '.                      
009700                                                                          
009800   03  GODK-STATUSKODER.                                                  
009900     05  GODK-STATUS OCCURS 3 INDEXED BY STATUS-IX PIC XX.                
010000     SKIP3                                                                
010100 01  NYCKLAR-TILL-DLI.                                                    
010200   03  W-WDG801KY-X.                                                      
010300     05  W-IDLTERM           PIC X(8).                                    
010400     05  W-TIREGDAT          PIC S9(7)   VALUE ZERO  COMP-3.              
010500     05  W-TIKLOCK-9KOMPL    PIC S9(9)   VALUE ZERO  COMP-3.              
010600     SKIP3                                                                
010700 01  SSA1                    PIC X(64).                                   
010800 01  SSA2                    PIC X(64).                                   
010900     EJECT                                                                
011000 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDG801'.           
011100                                                                          
011200 01  DLI-IO-WDG801.                                                       
011300*  03  -COPY WDG801  -PRE WDG8-.                                          
011400     EJECT                                                                
011500 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDG811'.           
011600                                                                          
011700 01  DLI-IO-WDG811.                                                       
011800*  03  -COPY WDG811  -PRE WDG8-.                                          
011900     EJECT                                                                
012000*                            IMS FUNKTIONSKODER                           
012100*01    -COPY W0003                                                        
012200                                                                          
012300     EJECT                                                                
012400 LINKAGE SECTION.                                                         
012500                                                                          
012600 01  -COPY W006PRTY -PRE LINK-                                            
012700     EJECT                                                                
012800 01  LINK-CALL-TYP           PIC X(5).                                    
012900     SKIP2                                                                
013000 01  LINK-IDPRTLST           PIC X(8).                                    
013100     SKIP2                                                                
013200*01  -COPY W0009 -PRE ALT-.                                               
013300     EJECT                                                                
013400*01  -COPY W0008 -PRE WDG8-                                               
013500     05  FILLER              PIC X.                                       
013600     SKIP2                                                                
013700 01  LINK-IDENT.                                                          
013800   03  LINK-IDLIST           PIC X(10).                                   
013900   03  FILLER                PIC X(18).                                   
014000     SKIP2                                                                
014100 01  LINK-SKIP               PIC S9(3) COMP-3.                            
014200     SKIP2                                                                
014300 01  LINK-RAD                PIC X(132).                                  
014400     EJECT                                                                
014500 PROCEDURE DIVISION USING  LINK-PRTY-W006PRTY                             
014600                           LINK-CALL-TYP                                  
014700                           LINK-IDPRTLST                                  
014800                           ALT-PCB                                        
014900                           WDG8-PCB                                       
015000                           LINK-IDENT                                     
015100                           LINK-SKIP                                      
015200                           LINK-RAD.                                      
015300 STYR SECTION.                                                            
015400                                                                          
015500     EVALUATE LINK-CALL-TYP                                               
015600       WHEN PRT-OPEN  MOVE HIGH-VALUE TO W-IDPRTLST                       
015700                                         W-IDPRTLST-SPAR                  
015800                                         W-IDLIST                         
015900                                                                          
016000       WHEN PRT-WRITE PERFORM B-WRITE                                     
016100                                                                          
016200       WHEN PRT-PURGE PERFORM S03-SKRIV                                   
016300                      IF W-FAX-SW = JA                                    
016400                        PERFORM IMS-INSERT-SPOOL-FAX                      
016500                        MOVE NEJ TO W-FAX-SW                              
016600                      END-IF                                              
016700                      MOVE JA TO W-CHANGE-SW                              
016800                                                                          
016900       WHEN PRT-CLOSE PERFORM S03-SKRIV                                   
017000                      IF W-FAX-SW = JA                                    
017100                        PERFORM IMS-INSERT-SPOOL-FAX                      
017200                        MOVE NEJ TO W-FAX-SW                              
017300                      END-IF                                              
017400                      MOVE JA TO W-CHANGE-SW                              
017500     END-EVALUATE                                                         
017600                                                                          
017700     MOVE ZERO TO RETURN-CODE                                             
017800     GOBACK                                                               
017900     .                                                                    
018000     EJECT                                                                
018100 B-WRITE SECTION.                                                         
018200*                         LINK-SKIP > 900 = NYSIDA                        
018300     MOVE LINK-SKIP TO W-SKIP                                             
018400     IF LINK-SKIP > W-NYSIDA                                              
018500       PERFORM S03-SKRIV                                                  
018600       IF LINK-IDLIST NOT = W-IDLIST                                      
018700         MOVE LINK-IDENT TO W-IDENT                                       
018800         MOVE JA TO W-PRT-NYLISTA                                         
018900       END-IF                                                             
019000       IF     LINK-IDPRTLST      NOT = W-IDPRTLST-SPAR                    
019100           OR LINK-PRTY-KDCOPIES NOT = PRTY-KDCOPIES                      
019200           OR LINK-PRTY-IDTFX    NOT = PRTY-IDTFX                         
019300         IF W-FAX-SW = JA                                                 
019400           PERFORM IMS-INSERT-SPOOL-FAX                                   
019500           MOVE NEJ TO W-FAX-SW                                           
019600         END-IF                                                           
019700         PERFORM BA-NYPRINTER                                             
019800         MOVE JA TO W-CHANGE-SW                                           
019900         MOVE JA TO W-PRT-NYLISTA                                         
020000       END-IF                                                             
020100       PERFORM S01-TAB-INIT                                               
020200     ELSE                                                                 
020300       IF LINK-SKIP > +800                                                
020400         MOVE W-SKIP TO W-IX                                              
020500       ELSE                                                               
020600         ADD LINK-SKIP TO W-IX                                            
020700       END-IF                                                             
020800       IF W-IX > W-MAX-IX                                                 
020900         MOVE W-MAX-IX TO W-IX                                            
021000         PERFORM S03-SKRIV                                                
021100         PERFORM S01-TAB-INIT                                             
021200       END-IF                                                             
021300     END-IF                                                               
021400     IF PRTY-IDPRTSPO = 'SPO-A4S '                                        
021500       MOVE LINK-RAD TO SPOOL-A4S-DATA (W-IX)                             
021600     ELSE                                                                 
021700       MOVE LINK-RAD TO SPOOL-RAD-DATA (W-IX)                             
021800     END-IF                                                               
021900     .                                                                    
022000     EJECT                                                                
022100 BA-NYPRINTER SECTION.                                                    
022200                                                                          
022300     CALL VIMSID USING W-VIMSID                                           
022400     MOVE +900 TO W-NYSIDA                                                
022500     IF LINK-IDPRTLST = 'W475FAX '                                        
022600        AND (LINK-PRTY-IDPFDEF = 'W475FA  ' OR SPACE)                     
022700       MOVE 'FAXNOP  '    TO PRT-IDPRTLST                                 
022800                             W-IDPRTLST                                   
022900     ELSE                                                                 
023000       MOVE LINK-IDPRTLST TO PRT-IDPRTLST                                 
023100                             W-IDPRTLST                                   
023200     END-IF                                                               
023300     MOVE LINK-IDPRTLST TO W-IDPRTLST-SPAR                                
023400     MOVE '003' TO PRT-KDCALL                                             
023500     CALL W006PRT USING PRT-W006PRT                                       
023600     MOVE PRT-BEPRTLST TO W-PRTTYP                                        
023700     MOVE LINK-PRTY-W006PRTY TO PRTY-W006PRTY                             
023800     IF PRTY-IDPFDEF = 'W475FA  '                                         
023900       MOVE SPACE TO PRTY-IDPFDEF                                         
024000     END-IF                                                               
024100     IF PRTY-KDCOPIES NUMERIC                                             
024200       MOVE PRTY-KDCOPIES TO SPOOL-COPIES                                 
024300     ELSE                                                                 
024400       MOVE '1'      TO SPOOL-COPIES                                      
024500     END-IF                                                               
024600     EVALUATE PRTY-KDFORMS                                                
024700       WHEN 'B'       MOVE 'F001' TO SPOOL-FORMS                          
024800       WHEN 'C'       MOVE '4811' TO SPOOL-FORMS                          
024900       WHEN 'D'       MOVE 'VCAS' TO SPOOL-FORMS                          
025000       WHEN '2'       MOVE '2000' TO SPOOL-FORMS                          
025100       WHEN '3'       MOVE '3000' TO SPOOL-FORMS                          
025200       WHEN OTHER     MOVE 'STD ' TO SPOOL-FORMS                          
025300     END-EVALUATE                                                         
025400     IF PRTY-IDPRTSPO = 'SPO-A4S '                                        
025500        MOVE +85 TO W-RADL                                                
025600        MOVE +80 TO W-MAX-IX                                              
025700     ELSE                                                                 
025800       MOVE +137 TO W-RADL                                                
025900       MOVE +70 TO W-MAX-IX                                               
026000     END-IF                                                               
026100     IF W-PRTTYP = 'LASER' OR 'TERMO' OR 'ZEBRA'                          
026200       MOVE 'IAFP=N1M,PRTO=' TO SPOOL-IAFP                                
026300     ELSE                                                                 
026400       MOVE 'IAFP=A1M,PRTO=' TO SPOOL-IAFP                                
026500     END-IF                                                               
026600     MOVE PRT-IDNODE TO SPOOL-IDNODE                                      
026700     IF W-PRTTYP (1:3) = 'IBM'                                            
026800       IF PRTY-IDPFDEF = SPACE AND W-IDPRTLST (1:1) = 'W'                 
026900         MOVE W-IDPRTLST (1:6) TO PRTY-IDPFDEF                            
027000       END-IF                                                             
027100     END-IF                                                               
027200     IF PRTY-IDPFDEF NOT = SPACE                                          
027300       MOVE PRTY-IDPFDEF TO SPOOL-FORMDEF SPOOL-PAGEDEF                   
027400       MOVE 'A'  TO SPOOL-CLASS                                           
027500*            -- SET TO CLASS E FOR OLD JES2 PRINTERS                      
027600       IF PRT-IDNODE (1:4) = 'NJO0'                                       
027700         MOVE 'E'  TO SPOOL-CLASS                                         
027800       END-IF                                                             
027900                                                                          
028000       MOVE PRT-IDNODE  TO TEST-PRINTER-NAME                              
029000       IF W-IMSID = 'IMG0'                                                
029010         IF VX-PRINTER                                                    
029020* FÖR EV FRAMTIDA BEHOV                                                   
029030           MOVE 'W??.????.PSF)' TO SPOOL-USERLIB                          
029040           MOVE 13 TO SPOOL-USERLIB-LENGTH                                
029050         ELSE                                                             
029060           MOVE 'W.QASE.PSF)' TO SPOOL-USERLIB                            
029070           MOVE 11 TO SPOOL-USERLIB-LENGTH                                
029080         END-IF                                                           
030300       ELSE                                                               
031200         MOVE 'W.IGRT.PSF,W.PROD.PSF)' TO SPOOL-USERLIB                   
031300         MOVE 22 TO SPOOL-USERLIB-LENGTH                                  
031600       END-IF                                                             
031700       MOVE SPOOL-IBM-LASER TO SPOOL-OVR-PARAM                            
031800       COMPUTE SPOOL-IAFP-LL = LENGTH OF SPOOL-NORMAL-OPTIONS             
031900                             + LENGTH OF SPOOL-IBM-LASER-DEL1             
032000                             + SPOOL-USERLIB-LENGTH                       
032100     ELSE                                                                 
032200       COMPUTE SPOOL-IAFP-LL = LENGTH OF SPOOL-NORMAL-OPTIONS             
032300       MOVE 'A'  TO SPOOL-CLASS                                           
032400       IF PRT-IDNODE (1:3) = 'R31' OR 'N38'                               
032500         MOVE 'F001' TO SPOOL-FORMS                                       
032600       ELSE                                                               
032700         IF PRT-IDNODE = 'R3200371'                                       
032800           MOVE '4811' TO SPOOL-FORMS                                     
032900         END-IF                                                           
033000       END-IF                                                             
033100     END-IF                                                               
033200                                                                          
033300     IF W-IMSID = 'IM1P' OR 'IMG0'                                        
033400       CONTINUE                                                           
033500     ELSE                                                                 
033600       IF PRT-IDNODE = 'QSE09012' OR 'QSE09013'                           
033700                    OR 'NJOV1   ' OR 'NJOV2   '                           
033800         MOVE 'IAFP=A00,PRTO=' TO SPOOL-IAFP                              
033900         MOVE 'H' TO SPOOL-CLASS                                          
034000         MOVE ',OUTDISP(HOLD,HOLD)' TO SPOOL-OVR-PARAM1                   
034100         MOVE SPOOL-IBM-LASER       TO SPOOL-OVR-PARAM2                   
034200         COMPUTE SPOOL-IAFP-LL = SPOOL-IAFP-LL + 19                       
034300       END-IF                                                             
034400     END-IF                                                               
034500                                                                          
034600     COMPUTE SPOOL-OPT-LL  = SPOOL-IAFP-LL                                
034700                           + LENGTH OF SPOOL-START-OPTIONS                
034800     .                                                                    
034900     EJECT                                                                
035000 S01-TAB-INIT SECTION.                                                    
035100                                                                          
035200     MOVE +1 TO INDX                                                      
035300     IF PRTY-IDPRTSPO = 'SPO-A4S '                                        
035400       PERFORM 80 TIMES                                                   
035500         MOVE +85   TO SPOOL-A4S-RDW (INDX)                               
035600         MOVE ZERO  TO SPOOL-A4S-ZZ  (INDX)                               
035700         MOVE SPACE TO SPOOL-A4S     (INDX)                               
035800         ADD +1 TO INDX                                                   
035900       END-PERFORM                                                        
036000       MOVE '1' TO SPOOL-A4S-STYR (1)                                     
036100     ELSE                                                                 
036200       PERFORM 70 TIMES                                                   
036300         MOVE +137  TO SPOOL-RAD-RDW (INDX)                               
036400         MOVE ZERO  TO SPOOL-RAD-ZZ  (INDX)                               
036500         MOVE SPACE TO SPOOL-RAD     (INDX)                               
036600         ADD +1 TO INDX                                                   
036700       END-PERFORM                                                        
036800       MOVE '1' TO SPOOL-RAD-STYR (1)                                     
036900     END-IF                                                               
037000                                                                          
037100     IF W-SKIP > W-MAX-IX                                                 
037200       MOVE +1 TO W-IX                                                    
037300     ELSE                                                                 
037400       MOVE W-SKIP TO W-IX                                                
037500       IF W-PRTTYP = 'IBMLB' OR 'IBMLD'                                   
037600       AND  (PRTY-IDPFDEF(1:4) = 'W475')                                  
037700         ADD +1 TO W-IX                                                   
037800       END-IF                                                             
037900     END-IF                                                               
038000     MOVE JA TO W-SKRIV-SW                                                
038100     .                                                                    
038200     EJECT                                                                
038300 S03-SKRIV SECTION.                                                       
038400                                                                          
038500     IF W-SKRIV-SW = JA                                                   
038600       IF W-PRT-NYLISTA = JA                                              
038700         MOVE PRT-IDNODE TO WDG8-LIST-IDLTERM W-IDLTERM                   
038800         ACCEPT W-DATUM FROM DATE                                         
038900         MOVE W-DATUM TO WDG8-LIST-TIREGDAT W-TIREGDAT                    
039000         ACCEPT W-TIME FROM TIME                                          
039100         COMPUTE W-TIKLOCK-9KOMPL = W-NIOR - W-TIME                       
039200         MOVE W-IDLIST TO WDG8-LIST-IDLIST                                
039300         MOVE W-IDPRTLST TO WDG8-LIST-IDPRTLST                            
039400         MOVE NEJ TO WDG8-LIST-FLSKRIV                                    
039500         IF W-PRTTYP = 'NOPRT'                                            
039600             OR W-PRTTYP = 'IBMLC'                                        
039700             OR W-PRTTYP = 'IBMLD'                                        
039800           MOVE +0 TO WDG8-LIST-KVANTEX-PRINTAD                           
039900         ELSE                                                             
040000           MOVE +1 TO WDG8-LIST-KVANTEX-PRINTAD                           
040100         END-IF                                                           
040200         MOVE PRTY-W006PRTY TO WDG8-LIST-W006PRTY                         
040300         MOVE W-PRTTYP      TO WDG8-LIST-IDPRTTYP                         
040400         MOVE W-IDPGM       TO WDG8-LIST-IDPGM                            
040500         MOVE W-RADL        TO WDG8-LIST-KVLL                             
040600         MOVE +0 TO W-IDSID                                               
040700         MOVE HIGH-VALUE TO STATUS-WS                                     
040800         PERFORM UNTIL STATUS-WS = SPACE                                  
040900           SUBTRACT +1 FROM W-TIKLOCK-9KOMPL                              
041000           MOVE W-TIKLOCK-9KOMPL TO WDG8-LIST-TIKLOCK-9KOMPL              
041100           PERFORM IMS-ISRT-WDG8-ROT                                      
041200         END-PERFORM                                                      
041300         MOVE NEJ TO W-PRT-NYLISTA                                        
041400       END-IF                                                             
041500       ADD +1 TO W-IDSID                                                  
041600       MOVE W-IDSID TO WDG8-SID-IDSID                                     
041700       COMPUTE SPOOL-RAD-BDW = W-RADL * W-IX + 4                          
041800       PERFORM S03-SAVE-PAGE                                              
041900       PERFORM IMS-ISRT-WDG8-SIDAN                                        
042000       IF PRT-IDNODE = 'QSE09012' OR 'QSE09013'                           
042100         CONTINUE                                                         
042200       ELSE                                                               
042300         IF W-PRTTYP = 'LASER' OR 'TERMO' OR 'ZEBRA'                      
042400*                    STYRTECKEN TAS BORT FRÅN POS 1 I ALLA RADER          
042500           MOVE +1 TO INDX                                                
042600           IF PRTY-IDPRTSPO = 'SPO-A4S '                                  
042700             PERFORM 80 TIMES                                             
042800               MOVE SPOOL-A4S-DATA (INDX) TO SPOOL-A4S (INDX)             
042900               ADD +1 TO INDX                                             
043000             END-PERFORM                                                  
043100           ELSE                                                           
043200             PERFORM 70 TIMES                                             
043300               MOVE SPOOL-RAD-DATA (INDX) TO SPOOL-RAD (INDX)             
043400               ADD +1 TO INDX                                             
043500             END-PERFORM                                                  
043600           END-IF                                                         
043700         END-IF                                                           
043800       END-IF                                                             
043900       INSPECT SPOOL-RAD-AREA REPLACING ALL '*COPY*' BY SPACE             
044000       IF W-PRTTYP = 'NOPRT'                                              
044100           OR W-PRTTYP = 'IBMLC'                                          
044200           OR W-PRTTYP = 'IBMLD'                                          
044300         CONTINUE                                                         
044400       ELSE                                                               
044500         IF W-CHANGE-SW = JA                                              
044600           IF PRTY-IDTFX NOT = SPACE                                      
044700               AND PRT-IDNODE (1:3) = 'FAX'                               
044800             MOVE 'QSERFAX ' TO SPOOL-IDNODE                              
044900             MOVE 'A'  TO SPOOL-CLASS                                     
045000             MOVE '1'  TO SPOOL-COPIES                                    
045100             MOVE +65  TO SPOOL-FAX-RDW  (1)                              
045200             MOVE ZERO TO SPOOL-FAX-ZZ   (1)                              
045300             MOVE '1'  TO SPOOL-FAX-STYR (1)                              
045400             EVALUATE W-IMSID                                             
045500               WHEN 'IMG0'                                                
045600               WHEN 'IM1P'                                                
045700                 MOVE SPACE TO SPOOL-FAX-DATA (1)                         
045800                 STRING '*AUTOFAX*<TOFAXNUM:' PRTY-IDTFX                  
045900                     DELIMITED BY SIZE INTO SPOOL-FAX-DATA (1)            
046000                 INSPECT SPOOL-FAX-DATA (1)                               
046100                     REPLACING FIRST SPACE BY '>'                         
046200               WHEN OTHER                                                 
046300                 MOVE '*AUTOFAX* <TOFAXNUM:62612>'                        
046400                     TO SPOOL-FAX-DATA (1)                                
046500             END-EVALUATE                                                 
046600             MOVE +65  TO SPOOL-FAX-RDW  (2)                              
046700             MOVE ZERO TO SPOOL-FAX-ZZ   (2)                              
046800             MOVE ' '  TO SPOOL-FAX-STYR (2)                              
046900             MOVE '<FROMNAME:PULS FAX DISTRIBUTION SYSTEM>'               
047000                  TO SPOOL-FAX-DATA (2)                                   
047100             MOVE +65  TO SPOOL-FAX-RDW  (3)                              
047200             MOVE ZERO TO SPOOL-FAX-ZZ   (3)                              
047300             MOVE ' '  TO SPOOL-FAX-STYR (3)                              
047400             MOVE '<TONAME:FAX RECEIVER>'                                 
047500                  TO SPOOL-FAX-DATA (3)                                   
047600             MOVE +1 TO INDX                                              
047700             PERFORM 5 TIMES                                              
047800               MOVE +65  TO SPOOL-FAX-RDW  (INDX + 3)                     
047900               MOVE ZERO TO SPOOL-FAX-ZZ   (INDX + 3)                     
048000               MOVE ' '  TO SPOOL-FAX-STYR (INDX + 3)                     
048100               STRING '<NOTE:' PRTY-TEFAX (INDX) '>   '                   
048200                 DELIMITED BY SIZE INTO SPOOL-FAX-DATA (INDX + 3)         
048300               ADD +1 TO INDX                                             
048400             END-PERFORM                                                  
048500             MOVE +65  TO SPOOL-FAX-RDW  (9)                              
048600             MOVE ZERO TO SPOOL-FAX-ZZ   (9)                              
048700             MOVE ' '  TO SPOOL-FAX-STYR (9)                              
048800             MOVE '<DELETELASTPAGE>'                                      
048900                  TO SPOOL-FAX-DATA (9)                                   
049000             MOVE JA TO W-FAX-SW                                          
049100           END-IF                                                         
049200           PERFORM IMS-CHANGE-SPOOL                                       
049300         END-IF                                                           
049400         PERFORM IMS-INSERT-SPOOL                                         
049500       END-IF                                                             
049600       MOVE NEJ TO W-SKRIV-SW                                             
049700     END-IF                                                               
049800     .                                                                    
049900     EJECT                                                                
050000 S03-SAVE-PAGE SECTION.                                                   
050100                                                                          
050200*                      TAR BORT TOMMA RADER OCH GÖR OM SIDAN              
050300*                      TILL ETT VB-SEGMENT                                
050400                                                                          
050500     MOVE SPOOL-RAD-BDW TO WDG8-SID-KVLL-BDW                              
050600     MOVE 01    TO WDG8-SID-IDPRTRAD (1)                                  
050700     MOVE SPACE TO WDG8-SID-TEPRTRAD (1)                                  
050800     MOVE +1 TO INDX                                                      
050900     MOVE +0 TO INDX2                                                     
051000     IF PRTY-IDPRTSPO = 'SPO-A4S'                                         
051100       MOVE SPOOL-A4S-RDW (1) TO WDG8-SID-KVLL-RDW                        
051200       PERFORM 80 TIMES                                                   
051300         IF SPOOL-A4S-DATA (INDX) > SPACE                                 
051400           ADD +1 TO INDX2                                                
051500           MOVE SPOOL-A4S-DATA (INDX)                                     
051600                TO WDG8-SID-TEPRTRAD (INDX2)                              
051700           MOVE INDX TO WDG8-SID-IDPRTRAD (INDX2)                         
051800         END-IF                                                           
051900         ADD +1 TO INDX                                                   
052000       END-PERFORM                                                        
052100     ELSE                                                                 
052200       MOVE SPOOL-RAD-RDW (1) TO WDG8-SID-KVLL-RDW                        
052300       PERFORM 70 TIMES                                                   
052400         IF SPOOL-RAD-DATA (INDX) > SPACE                                 
052500           ADD +1 TO INDX2                                                
052600           MOVE SPOOL-RAD-DATA (INDX)                                     
052700                TO WDG8-SID-TEPRTRAD (INDX2)                              
052800           MOVE INDX TO WDG8-SID-IDPRTRAD (INDX2)                         
052900         END-IF                                                           
053000         ADD +1 TO INDX                                                   
053100       END-PERFORM                                                        
053200     END-IF                                                               
053300     IF INDX2 = ZERO                                                      
053400       MOVE +1 TO INDX2                                                   
053500     END-IF                                                               
053600     COMPUTE WDG8-SID-KVLL = INDX2 * 134 + 8                              
053700     .                                                                    
053800     EJECT                                                                
053900* IMS SEKTIONER                                                           
054000                                                                          
054100 IMS-CHANGE-SPOOL SECTION.                                                
054200                                                                          
054300     IF W-PRTTYP = 'NOPRT'                                                
054400         OR W-PRTTYP = 'IBMLC'                                            
054500         OR W-PRTTYP = 'IBMLD'                                            
054600       CONTINUE                                                           
054700     ELSE                                                                 
054800       MOVE SPACE TO GODK-STATUSKODER                                     
054900       CALL CBLTDLI USING CHNG ALT-PCB SPOOL-OPT-DEST                     
055000                               SPOOL-OPTIONS SPOOL-FEEDBACK               
055100       MOVE ALT-STATUS-CODE TO STATUS-WS                                  
055200       PERFORM IMS-STATUSKONTROLL-SPOOL                                   
055300       MOVE NEJ TO W-CHANGE-SW                                            
055400     END-IF                                                               
055500     .                                                                    
055600                                                                          
055700                                                                          
055800 IMS-INSERT-SPOOL SECTION.                                                
055900                                                                          
056000     MOVE SPACE TO GODK-STATUSKODER                                       
056100     CALL CBLTDLI USING ISRT ALT-PCB SPOOL-RAD-AREA                       
056200     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
056300     PERFORM IMS-STATUSKONTROLL-SPOOL                                     
056400     .                                                                    
056500                                                                          
056600                                                                          
056700 IMS-INSERT-SPOOL-FAX SECTION.                                            
056800                                                                          
056900     MOVE SPACE TO GODK-STATUSKODER                                       
057000     CALL CBLTDLI USING ISRT ALT-PCB SPOOL-FAX-AREA                       
057100     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
057200     PERFORM IMS-STATUSKONTROLL-SPOOL                                     
057300     .                                                                    
057400                                                                          
057500                                                                          
057600     EJECT                                                                
057700                                                                          
057800 IMS-ISRT-WDG8-ROT SECTION.                                               
057900     IF WDG8-DBD-NAME = 'WDG8'                                            
058000       MOVE 'WDG801   ' TO SSA1                                           
058100     ELSE                                                                 
058200       MOVE 'WLLISB01 ' TO SSA1                                           
058300     END-IF                                                               
058400     MOVE '  II' TO GODK-STATUSKODER                                      
058500     CALL CBLTDLI USING ISRT WDG8-PCB DLI-IO-WDG801 SSA1                  
058600     MOVE WDG8-STATUS-CODE TO STATUS-WS                                   
058700     PERFORM IMS-STATUSKONTROLL                                           
058800     .                                                                    
058900                                                                          
059000                                                                          
059100 IMS-ISRT-WDG8-SIDAN SECTION.                                             
059200     IF WDG8-DBD-NAME = 'WDG8'                                            
059300       STRING 'WDG801  (WDG801KY =' W-WDG801KY-X ')'                      
059400              DELIMITED BY SIZE INTO SSA1                                 
059500       MOVE 'WDG811   ' TO SSA2                                           
059600     ELSE                                                                 
059700       STRING 'WLLISB01(WDG801KY =' W-WDG801KY-X ')'                      
059800              DELIMITED BY SIZE INTO SSA1                                 
059900       MOVE 'WLLISB11 ' TO SSA2                                           
060000     END-IF                                                               
060100     MOVE '  ' TO GODK-STATUSKODER                                        
060200     CALL CBLTDLI USING ISRT WDG8-PCB DLI-IO-WDG811 SSA1 SSA2             
060300     MOVE WDG8-STATUS-CODE TO STATUS-WS                                   
060400     PERFORM IMS-STATUSKONTROLL                                           
060500     .                                                                    
060600     EJECT                                                                
060700 IMS-STATUSKONTROLL-SPOOL SECTION.                                        
060800                                                                          
060900     SET STATUS-IX TO 1                                                   
061000     SEARCH GODK-STATUS                                                   
061100       AT END                                                             
061200         MOVE 'FEL FRÅN IMS SPOOL-INTERFACE SE SPOOL-FEEDBACK'            
061300           TO FELTEXT                                                     
061400         CALL FELLOG                                                      
061500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
061600         CONTINUE                                                         
061700     END-SEARCH                                                           
061800     .                                                                    
061900                                                                          
062000                                                                          
062100 IMS-STATUSKONTROLL SECTION.                                              
062200                                                                          
062300     SET STATUS-IX TO 1                                                   
062400     SEARCH GODK-STATUS                                                   
062500       AT END                                                             
062600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
062700         DELIMITED BY SIZE INTO FELTEXT                                   
062800         CALL FELLOG                                                      
062900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
063000         CONTINUE                                                         
063100     END-SEARCH                                                           
064000     .                                                                    
